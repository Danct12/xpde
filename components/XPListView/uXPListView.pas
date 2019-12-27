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
unit uXPListView;

interface

{ TODO : Add more functionality }

uses Classes, Types, QControls,
     QStdCtrls, QGraphics, QExtCtrls,
     uGraphics, Qt, uXPImageList,
     QForms, QFileCtrls, Sysutils,
     uResample, uCommon;

type

    TXPListViewGetImageIndexEvent=function (Sender:TObject; const itemindex:integer): integer of object;
    TXPListViewGetItemCaptionEvent=function (Sender:TObject; const itemindex:integer): string of object;
    TXPListViewGetItemPosEvent=procedure (Sender:TObject; const itemindex:integer; var pos: TPoint) of object;
    TXPListViewSetItemPosEvent=procedure (Sender:TObject; const itemindex:integer; pos: TPoint) of object;

    TXPListView=class(TCustomControl)
    private
        FBorderStyle: TBorderStyle;
        FBorderWidth: integer;
        FImageList: TXPImageList;
        FIconHSpacing: integer;
        FIconVSpacing: integer;
        FItemCount: integer;
        FOnGetImageIndex: TXPListViewGetImageIndexEvent;
        FOnGetItemPos: TXPListViewGetItemPosEvent;
        FOnGetItemCaption: TXPListViewGetItemCaptionEvent;
        FSelectedItem: integer;
        FBackground: TBitmap;
        FFirstTime: boolean;
        FButtonDown: boolean;
        FInMove: boolean;
        
        { TODO : Normalize these vars, this looks like sh... }
        old_x: integer;
        item_x: integer;
        old_y: integer;
        item_y: integer;
        lastrect: TRect;
        FOnSetItemPos: TXPListViewSetItemPosEvent;
        procedure SetBorderStyle(const Value: TBorderStyle);
        procedure SetBorderWidth(const Value: integer);
        procedure SetImageList(const Value: TXPImageList);
        procedure SetIconHSpacing(const Value: integer);
        procedure SetIconVSpacing(const Value: integer);
        procedure SetItemCount(const Value: integer);
        procedure SetOnGetImageIndex(const Value: TXPListViewGetImageIndexEvent);
        procedure SetOnGetItemPos(const Value: TXPListViewGetItemPosEvent);
        procedure SetOnGetItemCaption(const Value: TXPListViewGetItemCaptionEvent);
        procedure SetSelectedItem(const Value: integer);
        procedure SetOnSetItemPos(const Value: TXPListViewSetItemPosEvent);
    protected
        function WidgetFlags: Integer; override;
        procedure BorderStyleChanged; dynamic;
        procedure MouseDown(button: TMouseButton; Shift: TShiftState; X, Y: integer);override;
        procedure dblclick;override;
        procedure MouseMove(Shift: TShiftState; X, Y: integer);override;
        procedure MouseUp(button: TMouseButton; Shift: TShiftState; X, Y: integer);override;
        function GetClientOrigin: TPoint; override;
        function GetClientRect: TRect; override;
    public
        procedure paint;override;
        procedure drawitem(const itemindex: integer; toforeground:boolean=true; tobackground: boolean=true; x:integer=-1; y:integer=-1; cache: boolean=true);
        procedure redraw(const itemindex:integer=-1;const full:boolean=false);
        procedure setbackgroundimage(const filename:string; const amethod:integer);
        procedure setbackgroundcolor(color:TColor);
        procedure getAlignedCoords(var x, y: integer);
        function getdefaultposition(const itemIndex:integer):TPoint;
        function getItemRect(const itemIndex:integer):TRect;
        function getIconRect(const itemIndex: integer): TRect;        
        property Bitmap;
        constructor Create(AOwner:TComponent);override;
        destructor Destroy;override;
    published
        property Font;
        property PopupMenu;
        property SelectedItem: integer read FSelectedItem write SetSelectedItem;
        property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
        property BorderWidth: integer read FBorderWidth write SetBorderWidth;
        property ImageList: TXPImageList read FImageList write SetImageList;
        property ItemCount: integer read FItemCount write SetItemCount;
        property IconHSpacing: integer read FIconHSpacing write SetIconHSpacing;
        property IconVSpacing: integer read FIconVSpacing write SetIconVSpacing;
        property OnGetImageIndex: TXPListViewGetImageIndexEvent read FOnGetImageIndex write SetOnGetImageIndex;
        property OnGetItemCaption: TXPListViewGetItemCaptionEvent read FOnGetItemCaption write SetOnGetItemCaption;
        property OnGetItemPos: TXPListViewGetItemPosEvent read FOnGetItemPos write SetOnGetItemPos;
        property OnSetItemPos: TXPListViewSetItemPosEvent read FOnSetItemPos write SetOnSetItemPos;
    end;

implementation

{ TXPListView }


procedure TXPListView.BorderStyleChanged;
begin
    //Notification
end;

constructor TXPListView.Create(AOwner: TComponent);
begin
    inherited;
    FFirstTime:=false;
    FButtonDown:=false;
    FInMove:=false;
    FBackground:=TBitmap.create;
    FSelectedItem:=-1;
    FOnGetImageIndex:=nil;
    FOnGetItemPos:=nil;
    FOnSetItemPos:=nil;    
    FOnGetItemCaption:=nil;
    IconHSpacing:=43;
    IconVSpacing:=43;
    FImageList:=nil;
    FBorderStyle:=bsSunken3D;
    FBorderWidth:=1;
    color:=clWindow;
end;

destructor TXPListView.Destroy;
begin
  FBackground.free;
  inherited;
end;

function TXPListView.GetClientOrigin: TPoint;
begin
  Result.X := FBorderWidth*2;
  Result.Y := Result.X;
  QWidget_mapToGlobal(Handle, @Result, @Result);
end;

function TXPListView.GetClientRect: TRect;
var
  FW: Integer;
begin
  Result := inherited GetClientRect;
  FW := FBorderWidth*2;
  InflateRect(Result, -FW, -FW);
end;

//Returns the default position for an item
function TXPListView.getdefaultposition(const itemIndex: integer): TPoint;
var
    cx,cy:integer;
    item: integer;
begin
    { TODO : Return a "free" position instead a position depending on itemIndex }
    result:=Point(0,0);
    cx:=0;
    item:=0;
    while cx<width-32 do begin
        cy:=0;
        while cy<height-32 do begin
            if (itemIndex<=item) then begin
                GetAlignedCoords(cx,cy);
                result.x:=cx;
                result.y:=cy;
                exit;
            end;
            inc(item);
            inc(cy, (FIconVSpacing*2)-5);
        end;
        inc(cx, (FIconHSpacing*2)-5);
    end;
end;

//Returns coordinates aligned to the grid
procedure TXPListView.getAlignedCoords(var x, y: integer);
var
    dx: integer;
    dy: integer;
const
    w=32;
    h=32;
begin
    { TODO : The control must also allow free positioning }
    dx:=(FIconHSpacing*2)-6;
    if dx>200 then dx:=200;
    x:=x+w;

    dy:=(FIconVSpacing*2)-5;
    if dy>190 then dy:=190;
    y:=y+h;

    x:=(trunc(x / dx) * dx)+(FIconHSpacing div 2)+3;
    y:=(trunc(y / dy) * dy)+2;
end;


procedure TXPListView.paint;
begin
    //The control uses the bitmap property to draw itself automatically
    //so there is only need to paint the control for the first time
    if not FFirstTime then begin
        FFirstTime:=true;
        redraw;
    end;
end;


procedure TXPListView.redraw(const itemindex:integer=-1;const full:boolean=false);
var
    i: integer;
begin
    //If a paint event has been received
    if FFirsttime then begin


        //Free method, just to make the desktop work
        //The list view needs another paint methods

        { TODO : Implement another draw methods (icon, list, details, etc) }
        if (assigned(FImageList)) then begin
            //Sets the background
            FImageList.Background:=FBackground;

            //Draws all items or just one
            if (itemindex=-1) then begin
                if (full) then bitmap.assign(FBackground);
                for i:=0 to FItemCount-1 do begin
                        drawitem(i, false, true,-1, -1, false);
                end;
                canvas.Draw(0,0,bitmap);
            end
            else drawitem(itemindex);
        end;
    end;
end;


//Sets the background image
procedure TXPListView.setbackgroundimage(const filename: string; const amethod: integer);
var
    b: TBitmap;
    c: TBitmap;
    ax,ay:integer;
begin
    if fileexists(filename) then begin
        b:=TBitmap.create;
        c:=TBitmap.create;
        try
            b.loadfromfile(filename);
            bitmap.canvas.Brush.color:=self.color;
            bitmap.canvas.pen.color:=self.color;
            bitmap.width:=width;
            bitmap.height:=height;

            c.width:=width;
            c.height:=height;
            bitmap.Canvas.Rectangle(0,0,width,height);

            //Depending on the method
            case amethod of
                0: begin
                    ax:=(bitmap.width-b.Width) div 2;
                    ay:=(bitmap.height-b.height) div 2;
                    bitmap.Canvas.draw(ax,ay,b);
                end;
                1: begin
                    ax:=0;
                    while (ax<bitmap.width) do begin
                        ay:=0;
                        while (ay<bitmap.height) do begin
                            bitmap.Canvas.draw(ax,ay,b);
                            inc(ay,b.height);
                        end;
                        inc(ax,b.width);
                    end;
                end;
                2: begin
                    Strecth(b,c,resampleFilters[4].filter,resamplefilters[4].width);
                    bitmap.assign(c);
                end;
            end;

            //This is the background for graphic operations
            FBackground.assign(bitmap);

        finally
            c.free;
            b.free;
        end;
    end;
end;

procedure TXPListView.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then begin
    FBorderStyle := Value;
    Invalidate;
    BorderStyleChanged;
  end;
end;

procedure TXPListView.SetBorderWidth(const Value: integer);
begin
    if (Value<>FBorderWidth) then begin
        FBorderWidth := Value;
        invalidate;
    end;
end;

procedure TXPListView.SetIconHSpacing(const Value: integer);
begin
    if value<>FIconHSpacing then begin
        FIconHSpacing := Value;
        invalidate;
    end;
end;

procedure TXPListView.SetIconVSpacing(const Value: integer);
begin
    if value<>FIconVSpacing then begin
        FIconVSpacing := Value;
        invalidate;
    end;
end;

procedure TXPListView.SetImageList(const Value: TXPImageList);
begin
    if value<>FImageList then begin
        FImageList := Value;
    end;
end;



procedure TXPListView.SetItemCount(const Value: integer);
begin
    if value<>FItemCount then begin
        FItemCount := Value;
        invalidate;
    end;
end;

procedure TXPListView.SetOnGetImageIndex(const Value: TXPListViewGetImageIndexEvent);
begin
    FOnGetImageIndex := Value;
end;

procedure TXPListView.SetOnGetItemPos(const Value: TXPListViewGetItemPosEvent);
begin
  FOnGetItemPos := Value;
end;

function TXPListView.WidgetFlags: Integer;
begin
    //This improves a bit repainting
    Result := Inherited WidgetFlags or Integer(WidgetFlags_WRepaintNoErase) or Integer(WidgetFlags_WResizeNoErase);
end;

procedure TXPListView.SetOnGetItemCaption(const Value: TXPListViewGetItemCaptionEvent);
begin
  FOnGetItemCaption := Value;
end;

//Returns the rect of an item
function TXPListView.getItemRect(const itemIndex: integer): TRect;
var
    pos: TPoint;
    caption: string;

    maxtext:integer;
    r: TRect;
    tx: integer;
    tw: integer;
    tl: integer;
    tt: integer;
    th: integer;

const
    w=32;
    h=32;
begin
    result:=Rect(0,0,0,0);

    { TODO : Refactor this part, posibly needs to optimize calculations, variables, etc }
    if (assigned(FImageList)) then begin
        if assigned(FOnGetImageIndex) then begin
            if assigned(FOnGetItemPos) then begin
                FOnGetItemPos(self, itemIndex, pos);
                if assigned(FOnGetItemCaption) then begin
                    caption:=FOnGetItemCaption(self, itemindex);
                    canvas.Font.Assign(self.font);

                    maxtext:=(w*2)+10;
                    r:=Rect(0,0,maxtext,canvas.TextHeight(caption));
                    canvas.TextExtent(caption,r,1036);
                    r.left:=r.Left-2;
                    r.right:=r.right+2;
                    tw:=r.Right-r.left;
                    th:=r.bottom-r.top;

                    tx:=((tw-w) div 2);
                    tl:=pos.x-tx;
                    tt:=pos.y+h+4;

                    r:=Rect(tl, tt, tl+tw, tt+th);
                    result.left:=min(r.left, pos.x);
                    result.top:=min(r.top,pos.y);
                    result.right:=max(r.right,pos.x+w);
                    result.bottom:=max(r.bottom, pos.y+h);
                end;
            end;
        end;
    end;
end;

//Returns the rect of the icon part
function TXPListView.getIconRect(const itemIndex: integer): TRect;
var
    pos: TPoint;

const
    w=32;
    h=32;
begin
    result:=Rect(0,0,0,0);

    { TODO : Refactor this part, posibly needs to optimize calculations, variables, etc }
    if (assigned(FImageList)) then begin
        if assigned(FOnGetImageIndex) then begin
            if assigned(FOnGetItemPos) then begin
                FOnGetItemPos(self, itemIndex, pos);
                result.left:=pos.x;
                result.top:=pos.y;
                result.right:=result.left+w;
                result.bottom:=result.top+h;
            end;
        end;
    end;
end;

procedure TXPListView.SetSelectedItem(const Value: integer);
var
    old_selected: integer;
begin
    if (value<>FSelectedItem) then begin
        old_selected:=FSelectedItem;
        FSelectedItem := Value;
        //Redraws items to show the selected one
        if (old_selected<>-1) and (old_selected<itemcount) then begin
            redraw(old_selected);
        end;
        if (FSelectedItem<>-1) and (fselecteditem<itemcount) then begin
            redraw(FSelectedItem);
        end;
    end;
end;

//Mouse down
procedure TXPListView.MouseDown(button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
    i: integer;
    r: TRect;
    p: TPoint;
    ns: integer;
begin
  inherited;

  { TODO : Optimize this by adding an ItemAtPos function }
  { TODO : Allow multiple selection }
  { TODO : Allow multiple item movements }
  p:=Point(x,y);
  ns:=-1;
  for i:=0 to FItemCount-1 do begin
    r:=getItemRect(i);
    if (PtInRect(r,p)) then begin
        ns:=i;
        break;
    end;
  end;
  SelectedItem:=ns;
  FButtonDown:=true;
  old_x:=x;
  old_y:=y;

  //Stores item coordinates for future icon movements
  if (FSelectedItem<>-1) then begin
    r:=getIconRect(i);
    item_x:=r.left;
    item_y:=r.Top;
  end;
end;


procedure TXPListView.drawitem(const itemindex: integer; toforeground:boolean=true; tobackground: boolean=true; x:integer=-1; y:integer=-1; cache: boolean=true);
var
    index:integer;
    pos: TPoint;
    caption: string;

    maxtext:integer;
    r: TRect;
    tx: integer;
    tw: integer;
    th: integer;
    tl: integer;
    tt: integer;
    tr: TRect;
    ar: TRect;
    b: TBitmap;

    ACanvas: TCanvas;
    dens: integer;
    mr: TRect;
    mb: TBitmap;

    ir: TRect;
    im: integer;

const
    w=32;
    h=32;

begin
    { TODO : Refactor to improve draw, this can be greatly improved, check it with PIII 500 }
    
    //Free method, just to make the desktop work
    //The list view needs another paint methods

    //Choose the canvas where to paint
    ACanvas:=bitmap.canvas;
        if assigned(FOnGetImageIndex) then begin

            //Get the image
            index:=FOnGetImageIndex(self, itemindex);
            FImageList.BackgroundColor:=color;
            if assigned(FOnGetItemPos) then begin
                //Get the position
                if (x<>-1) or (y<>-1) then pos:=Point(x,y)
                else FOnGetItemPos(self, itemindex, pos);

                dens:=0;

                { TODO : Refactor this! }
                if (FSelectedItem=itemindex) and (FInMove) then begin
                    //Nothing
                end
                else begin
                    FImageList.Background:=FBackground;
                    if (toforeground) then FImageList.Draw(Canvas,pos.x,pos.y,index, (FSelectedItem=itemindex), dens, cache);
                    if (tobackground) then FImageList.Draw(ACanvas,pos.x,pos.y,index, (FSelectedItem=itemindex), dens, cache);
                end;

                //Gets the caption
                if assigned(FOnGetItemCaption) then begin
                    caption:=FOnGetItemCaption(self, itemindex);
                    ACanvas.Font.Assign(self.font);

                    //Perform some calculations to get where to draw the caption
                    maxtext:=(w*2)+10;
                    r:=Rect(0,0,maxtext,ACanvas.TextHeight(caption));
                    ACanvas.TextExtent(caption,r,1036);
                    r.left:=r.Left-2;
                    r.right:=r.right+2;
                    ar:=r;
                    tw:=r.Right-r.left;
                    th:=r.bottom-r.top;

                    tx:=((tw-w) div 2);
                    tl:=pos.x-tx;
                    tt:=pos.y+h+4;

                    r:=Rect(tl, tt, tl+tw, tt+th);
                    b:=TBitmap.create;
                    try
                            b.width:=r.right-r.left;
                            b.height:=r.bottom-r.top;
                            tr:=rect(0,0,b.width,b.height);
                            b.canvas.Font.Assign(self.font);
                            b.canvas.CopyRect(rect(0,0,b.width,b.height),FBackground.canvas,r);
                            if (FSelectedItem<>itemindex) then begin
                                b.canvas.Font.color:=clGray;
                                b.canvas.TextRect(tr,1,1,caption,1036);
                            end
                            else begin
                                b.Canvas.Brush.Color:=clHighlight;
                                b.Canvas.pen.Color:=b.Canvas.Brush.color;
                                b.canvas.Rectangle(0,1,b.width,ar.bottom-1);
                            end;
                            b.canvas.Font.Assign(self.font);
                            b.canvas.TextRect(tr,0,0,caption,1036);

                            ir.left:=min(pos.x,r.left);
                            ir.top:=min(pos.y,r.top);
                            ir.right:=max(pos.x+32,r.right);
                            ir.bottom:=max(pos.y+32,r.bottom);

                            //If the item is moving
                            if (FSelectedItem=itemindex) and (FInMove) then begin
                                mr.left:=min(lastrect.left, ir.left);
                                mr.top:=min(lastrect.top, ir.top);
                                mr.right:=max(lastrect.right, ir.right);
                                mr.bottom:=max(lastrect.bottom, ir.bottom);
                                mb:=TBitmap.create;
                                try
                                    mb.width:=mr.Right-mr.left;
                                    mb.height:=mr.bottom-mr.top;
                                    mb.canvas.CopyRect(rect(0,0,mb.width,mb.height),bitmap.canvas,mr);

                                    //This is to draw the caption transparent...
                                    { TODO : REFACTOR! }
                                    FImageList.Background:=mb;
                                    FImageList.Draw(mb.canvas,pos.x-mr.left, pos.y-mr.top,index, true, 12, false);
                                    im:=FImageList.Add(b);
                                    FImageList.Draw(mb.canvas,r.left-mr.left, r.top-mr.top,im, false, 12, false);
                                    FImageList.Delete(im);

                                    canvas.draw(mr.left,mr.top,mb);
                                finally
                                    mb.free;
                                end;
                            end
                            else begin
                                if (toforeground) then canvas.draw(r.left,r.top,b);
                                if (tobackground) then ACanvas.Draw(r.Left,r.top,b);
                            end;
                            lastrect:=ir;
                    finally
                        b.free;
                    end;
                end;
            end;
    end;
end;

procedure TXPListView.MouseMove(Shift: TShiftState; X, Y: integer);
var
    initial: boolean;
    ar: TRect;
begin
  inherited;
  initial:=false;
  if (FButtonDown) and (not FInMove) and (FSelectedItem<>-1) then begin
  { TODO : Make the threshold configurable by properties }
    if (abs(x-old_x)>=3) or (abs(y-old_y)>=3) then begin
        //Start moving the item
        FInMove:=true;
        initial:=true;
        lastrect:=getItemRect(FSelectedItem);
        ar:=lastrect;
    end;
  end;

  if FInMove then begin
        //Remove the item that it's being moved from the background
        if initial then bitmap.Canvas.CopyRect(ar,fbackground.Canvas,ar);
        drawitem(FSelectedItem,true,false, item_x+(x-old_x),item_y+(y-old_y));
  end;
end;

procedure TXPListView.MouseUp(button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
var
    p: TPoint;
    ax,ay: integer;
begin
  inherited;
  FButtonDown:=false;
  //Sets the item on the new position
  if FInMove then begin
    if (assigned(FOnSetItemPos)) then begin
        ax:=item_x+(x-old_x);
        ay:=item_y+(y-old_y);
        //Removing it from the moving position
        drawitem(FSelectedItem, true, false, ax,ay);
        getAlignedCoords(ax, ay);
        //Placing it at the aligned position
        p:=Point(ax,ay);
        drawitem(FSelectedItem, true, false, ax,ay);
        FInMove:=false;
        drawitem(FSelectedItem, true, true, ax,ay);
        FOnSetItemPos(self,FSelectedItem,p);
    end;
  end;
end;

procedure TXPListView.SetOnSetItemPos(const Value: TXPListViewSetItemPosEvent);
begin
  FOnSetItemPos := Value;
end;

procedure TXPListView.setbackgroundcolor(color: TColor);
begin
    //Sets the background to a fixed color
    { TODO : Investigate to reduce the memory usage when it's just a plain color, no need for a memory FBackground }
    self.color:=color;
    bitmap.canvas.Brush.color:=color;
    bitmap.canvas.pen.color:=color;
    bitmap.width:=width;
    bitmap.height:=height;

    bitmap.Canvas.Rectangle(0,0,width,height);

    FBackground.assign(bitmap);
end;

procedure TXPListView.dblclick;
begin
  inherited;
  //Cancels any movement
  FButtonDown:=false;
  FInMove:=false;
end;

end.
