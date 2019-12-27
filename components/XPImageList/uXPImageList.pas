{ *************************************************************************** }
{                                                                             }
{ This file is part of the XPde project                                       }
{                                                                             }
{ Copyright (c) 2002 Jose Leon Serna <ttm@xpde.com>                           }
{                                                                             }
{ This program is free software; you can redistribute it and/or               }
{ modify it under the terms of the GNU General Public                         }
{ License as published by the Free Software Foundation; either                }
{ version 2 of the License, or (at your option) any later version.            }
{                                                                             }
{ This program is distributed in the hope that it will be useful,             }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of              }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           }
{ General Public License for more details.                                    }
{                                                                             }
{ You should have received a copy of the GNU General Public License           }
{ along with this program; see the file COPYING.  If not, write to            }
{ the Free Software Foundation, Inc., 59 Temple Place - Suite 330,            }
{ Boston, MA 02111-1307, USA.                                                 }
{                                                                             }
{ *************************************************************************** }
unit uXPImageList;

interface

uses
    Classes, QGraphics, SysUtils,
    QImgList, QForms, uGraphics,
    uXPPNG, uCommon;

type
    TXPImageList=class(TComponent)
    private
        FImages:TList;
        FBackgroundColor: TColor;
        FBackground: TBitmap;
    FDefaultSystemDir: integer;
        function getCount: integer;
        procedure SetBackgroundColor(const Value: TColor);
        procedure SetBackground(const Value: TBitmap);
        procedure SetDefaultSystemDir(const Value: integer);
    public
        function Add(AImage:TBitmap):integer; overload;
        function Add(const filename: string): integer; overload;
        procedure AddImages(Value: TXPImageList);
        procedure Draw(Canvas: TCanvas; X, Y, Index: Integer; selected:boolean=false; dens:integer=0; cache:boolean=true);
        procedure Insert(Index: Integer; Image: TBitmap);
        procedure GetBitmap(Index: Integer; Image: TBitmap);
        function GetBitmapWidth(Index: Integer):integer;
        function GetBitmapHeight(Index: Integer):integer;
        procedure Delete(Index: Integer);
        procedure Move(CurIndex, NewIndex: Integer);
        procedure Replace(Index: Integer; AImage: TBitmap);
        procedure Clear;
        constructor Create(AOwner:TComponent);override;
        destructor Destroy; override;
    published
        property DefaultSystemDir: integer read FDefaultSystemDir write SetDefaultSystemDir;
        property Count: integer read getCount;
        property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
        property Background: TBitmap read FBackground write SetBackground;
    end;

implementation

{ TXPImageList }

//Adds an image to the list
function TXPImageList.Add(AImage: TBitmap):integer;
var
    b: TXPPNG;
begin
    b:=TXPPNG.create;
    b.assign(AImage);
    result:=FImages.add(b);
end;

//Adds an image from a file
function TXPImageList.Add(const filename: string): integer;
var
    b: TBitmap;
    fname: string;
begin
    fname:=filename;
    if (not fileexists(fname)) then begin
        fname:=getSystemInfo(FDefaultSystemDir)+'/'+extractfilename(filename);
    end;

    if (fileexists(fname)) then begin
        b:=TBitmap.create;
        try
            b.LoadFromFile(fname);
            result:=Add(b);
        finally
            b.free;
        end;
    end
    else result:=0;
end;

//Adds the images of another list to this list
procedure TXPImageList.AddImages(Value: TXPImageList);
var
    b: TBitmap;
    i: integer;
begin
    b:=TBitmap.create;
    try
        for i:=0 to value.count-1 do begin
            value.GetBitmap(i,b);
            add(b);
        end;
    finally
        b.free;
    end;
end;

//Clear the list
procedure TXPImageList.Clear;
var
    i: integer;
    b: TBitmap;
begin
    for i:=FImages.count-1 downto 0 do begin
        b:=FImages[i];
        b.free;
        FImages.Delete(i);
    end;
end;


//Constructor
constructor TXPImageList.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultSystemDir:=XP_NORMAL_SIZE_ICON_DIR;
  FImages:=TList.create;
  FBackgroundColor:=clNone;
  FBackground:=nil;
end;

//Delete an image from the list
procedure TXPImageList.Delete(Index: Integer);
var
    b: TBitmap;
begin
    b:=FImages[index];
    b.free;
    FImages.Delete(index);
end;

destructor TXPImageList.Destroy;
begin
  Clear;
  FImages.free;
  inherited;
end;

//Draw an image to a canvas
procedure TXPImageList.Draw(Canvas: TCanvas; X, Y, Index: Integer; selected:boolean=false; dens:integer=0; cache: boolean=true);
var
    b: TXPPNG;
begin
    //Get the image
    b:=fimages[index];

    //Set the back color
    if (FBackgroundColor<>clNone) then b.BackgroundColor:=FBackgroundColor;

    //Set the background
    if (assigned(FBackground)) and (not FBackground.Empty) then  begin
        b.Background.Canvas.CopyRect(rect(0,0,b.width,b.height),FBackground.Canvas,rect(x,y,x+b.Width,y+b.height));
    end;

    { TODO : Refactor cache handling }
    
    //Paint the image
    b.Selected:=selected;
    b.cache:=cache;
    b.paintToCanvas(Canvas,x,y, dens);
end;


//Returns an image of the list
procedure TXPImageList.GetBitmap(Index: Integer; Image: TBitmap);
var
    b: TBitmap;
begin
    if assigned(image) then begin
        b:=FImages[index];
        image.width:=b.width;
        image.height:=b.height;
        image.assign(b);
    end;
end;

//Return how many images
function TXPImageList.GetBitmapHeight(Index: Integer): integer;
var
    b: TBitmap;
begin
    b:=FImages[index];
    result:=b.height;
end;

function TXPImageList.GetBitmapWidth(Index: Integer): integer;
var
    b: TBitmap;
begin
    b:=FImages[index];
    result:=b.width;
end;

function TXPImageList.getCount: integer;
begin
    result:=FImages.Count;
end;

//Insert an image into the list
procedure TXPImageList.Insert(Index: Integer; Image: TBitmap);
var
    b: TBitmap;
begin
    b:=TBitmap.create;
    b.assign(image);
    FImages.insert(index,b);
end;

//Move an image
procedure TXPImageList.Move(CurIndex, NewIndex: Integer);
begin
    FImages.Move(curindex,newindex);
end;

//Replace an image
procedure TXPImageList.Replace(Index: Integer; AImage: TBitmap);
begin
    delete(index);
    insert(index,AImage);
end;

//Set the background bitmap
procedure TXPImageList.SetBackground(const Value: TBitmap);
begin
  FBackground := Value;
end;

//Set the background color
procedure TXPImageList.SetBackgroundColor(const Value: TColor);
begin
    if (value<>FBackgroundColor) then begin
       FBackgroundColor := Value;
    end;
end;

procedure TXPImageList.SetDefaultSystemDir(const Value: integer);
begin
  FDefaultSystemDir := Value;
end;

end.
