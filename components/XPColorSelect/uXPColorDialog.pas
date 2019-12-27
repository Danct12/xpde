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
unit uXPColorDialog;

interface

uses
  SysUtils, Types, Classes,
  Variants, QGraphics, QControls,
  QForms, QDialogs, QExtCtrls,
  QStdCtrls, uGraphics;

type
  TXPColorDialog = class(TForm)
    btnOther: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOtherClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormClick(Sender: TObject);
  private
    FSelectedColor: TColor;
    procedure drawSelected(r: TRect;acolor:TColor);
    procedure SetSelectedColor(const Value: TColor);
    { Private declarations }
  public
    { Public declarations }
    lastpx,lastpy:integer;
    lastx,lasty:integer;
    lastcolor:TColor;
    colors: array [0..3,0..4] of integer;
    property SelectedColor:TColor read FSelectedColor write SetSelectedColor;
  end;

var
  XPColorDialog: TXPColorDialog;

implementation

{$R *.xfm}

procedure TXPColorDialog.FormShow(Sender: TObject);
begin
    lastcolor:=FSelectedColor;
    width:=98;
end;

procedure TXPColorDialog.FormPaint(Sender: TObject);
var
    x,y:integer;
    r: TRect;
begin
    canvas.brush.color:=clButton;
    r3d(canvas,clientrect,false,true,false);
    for x:=0 to 3 do begin
        for y:=0 to 4 do begin
            r.left:=(x*23)+5;
            r.Top:=(y*23)+5;
            r.Right:=r.left+19;
            r.Bottom:=r.top+19;
            canvas.brush.color:=colors[x,y];
            r3d(canvas,r,false,false,true);
        end;
    end;

    canvas.brush.color:=FSelectedColor;
    r.left:=btnOther.BoundsRect.right+3;
    r.top:=btnOther.BoundsRect.top+2;
    r.Right:=r.left+19;
    r.Bottom:=r.top+19;
    r3d(canvas,r,false,false,true);

    r.left:=lastx-2;
    r.top:=lasty-2;
    r.right:=r.left+23;
    r.bottom:=r.top+23;
    drawSelected(r,lastcolor);

    canvas.pen.color:=clBtnShadow;
    canvas.moveto(btnOther.left,btnOther.top-4);
    canvas.lineto(width-5,btnOther.top-4);

    canvas.pen.color:=clBtnHighlight;
    canvas.moveto(btnOther.left,btnOther.top-3);
    canvas.lineto(width-5,btnOther.top-3);
    canvas.lineto(width-5,btnOther.top-4);
end;

procedure TXPColorDialog.FormCreate(Sender: TObject);
begin
    lastpx:=-1;
    lastpy:=-1;
    lastx:=btnOther.BoundsRect.right+3;
    lasty:=btnOther.BoundsRect.top+2;

    FSelectedColor:=clNone;
    colors[0,0]:=$ffffff;
    colors[1,0]:=$000000;
    colors[2,0]:=$c5c2c5;
    colors[3,0]:=$838183;

    colors[0,1]:=$0000ff;
    colors[1,1]:=$000083;
    colors[2,1]:=$00ffff;
    colors[3,1]:=$008183;

    colors[0,2]:=$00ff00;
    colors[1,2]:=$008200;
    colors[2,2]:=$ffff00;
    colors[3,2]:=$838100;

    colors[0,3]:=$ff0000;
    colors[1,3]:=$830000;
    colors[2,3]:=$ff00ff;
    colors[3,3]:=$830083;

    colors[0,4]:=$c5dec5;
    colors[1,4]:=$f6caa4;
    colors[2,4]:=$f6faff;
    colors[3,4]:=$a4a1a4;
end;

procedure TXPColorDialog.btnOtherClick(Sender: TObject);
begin
    modalresult:=mrAll;
end;

procedure TXPColorDialog.SetSelectedColor(const Value: TColor);
begin
  FSelectedColor := Value;
end;

procedure TXPColorDialog.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
    r:TRect;
    p: TPoint;
begin
    p.x:=x;
    p.Y:=y;
    x:=x div 23;
    y:=y div 23;
    if (x>=0) and (x<=3) and (y>=0) and (y<=4) then begin
        if (x<>lastpx) or (y<>lastpy) then begin
            lastpx:=x;
            lastpy:=y;
            canvas.brush.color:=clButton;
            canvas.brush.style:=bsClear;
            canvas.pen.color:=clButton;
            r.left:=lastx-2;
            r.top:=lasty-2;
            r.Right:=r.left+19+4;
            r.Bottom:=r.top+19+4;
            canvas.rectangle(r);
            inflaterect(r,-1,-1);
            canvas.rectangle(r);
            canvas.brush.style:=bsSolid;
            canvas.brush.color:=lastcolor;
            r3d(canvas,rect(lastx,lasty,lastx+19,lasty+19),false,false,true);

            r.left:=(x*23)+5-2;
            r.Top:=(y*23)+5-2;
            r.Right:=r.left+19+4;
            r.Bottom:=r.top+19+4;

            lastx:=r.left+2;
            lasty:=r.Top+2;
            lastcolor:=colors[x,y];

            drawSelected(r,colors[x,y]);
        end;
    end
    else begin
        r.left:=btnOther.BoundsRect.right+3;
        r.top:=btnOther.BoundsRect.top+2;
        r.Right:=r.left+19;
        r.Bottom:=r.top+19;
        if ptInRect(r,p) then begin
            lastpx:=-1;
            lastpy:=-1;
            canvas.brush.color:=clButton;
            canvas.brush.style:=bsClear;
            canvas.pen.color:=clButton;
            r.left:=lastx-2;
            r.top:=lasty-2;
            r.Right:=r.left+19+4;
            r.Bottom:=r.top+19+4;
            canvas.rectangle(r);
            inflaterect(r,-1,-1);
            canvas.rectangle(r);
            canvas.brush.style:=bsSolid;
            canvas.brush.color:=lastcolor;
            r3d(canvas,rect(lastx,lasty,lastx+19,lasty+19),false,false,true);

            r.left:=btnOther.BoundsRect.right+3-2;
            r.Top:=btnOther.BoundsRect.top+2-2;
            r.Right:=r.left+19+4;
            r.Bottom:=r.top+19+4;

            lastx:=r.left+2;
            lasty:=r.Top+2;
            lastcolor:=FSelectedColor;

            drawSelected(r,FSelectedColor);
        end;
    end;
end;

procedure TXPColorDialog.drawSelected(r: TRect;acolor:TColor);
begin
        canvas.pen.color:=clBlack;
        canvas.brush.color:=acolor;
        canvas.rectangle(r);

        inflaterect(r,-1,-1);
        canvas.pen.color:=clWhite;
        canvas.rectangle(r);

        inflaterect(r,-1,-1);
        canvas.pen.color:=clBlack;
        canvas.rectangle(r);
end;

procedure TXPColorDialog.FormClick(Sender: TObject);
begin
    SelectedColor:=lastcolor;
    modalresult:=mrOk;
end;

end.
