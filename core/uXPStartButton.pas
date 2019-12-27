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
unit uXPStartButton;

interface

uses
    Classes, QExtCtrls, uCommon,
    QControls, QForms, QGraphics,
    QDialogs, Sysutils, Types;

type
    TXPStartButton=class(TPanel)
    private
        FDown: boolean;
        FOver: TBitmap;
        FNormal: TBitmap;
        FPressed: TBitmap;
        FOnShowMenu: TNotifyEvent;
        procedure SetOnShowMenu(const Value: TNotifyEvent);
    public
        procedure release;
        procedure MouseDown(button: TMouseButton; Shift: TShiftState; X, Y: integer);override;
        procedure mouseenter(AControl:TControl);override;
        procedure mouseleave(AControl:TControl);override;
        procedure setup;
        constructor Create(AOwner:TComponent);override;
        destructor Destroy;override;
    published
        property OnShowMenu: TNotifyEvent read FOnShowMenu write SetOnShowMenu;
    end;


implementation

{ TXPStartButton }

constructor TXPStartButton.Create(AOwner: TComponent);
begin
  inherited;
  FDown:=false;
  FOnShowMenu:=nil;
  FOver:=TBitmap.create;
  FNormal:=TBitmap.create;
  FPressed:=TBitmap.create;
  setup;
end;


destructor TXPStartButton.Destroy;
begin
  FOver.free;
  FNormal.free;
  FPressed.free;
  inherited;
end;

procedure TXPStartButton.MouseDown(button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited;
  if (not FDown) then begin
    FDown:=true;
    bitmap.assign(FPressed);
    if assigned(FOnShowMenu) then FOnShowMenu(self);
  end
  else begin
    FDown:=false;
    if ptinrect(clientrect,point(x,y)) then bitmap.assign(FOver)
    else bitmap.assign(FNormal);
  end;
end;

procedure TXPStartButton.mouseenter(AControl: TControl);
begin
  inherited;
  if (not FDown) then bitmap.assign(FOver);
end;

procedure TXPStartButton.mouseleave(AControl: TControl);
begin
  inherited;
  if (not FDown) then bitmap.assign(FNormal);
end;

procedure TXPStartButton.release;
begin
    FDown:=false;

    if ptinrect(clientrect,ScreenToClient(mouse.CursorPos)) then bitmap.assign(FOver)
    else bitmap.assign(FNormal);

    SetMouseGrabControl(nil);
end;

procedure TXPStartButton.SetOnShowMenu(const Value: TNotifyEvent);
begin
  FOnShowMenu := Value;
end;

procedure TXPStartButton.setup;
var
    dir: string;
const
    st_x=24;
    st_y=7;
    title='StartMenu';
    style=[fsBold];
    fsize=11;
begin
    BevelOuter:=bvNone;
    dir:=getSystemInfo(XP_TASKBAR_DIR);
    FNormal.LoadFromFile(dir+'/start_button_normal.png');
    FNormal.canvas.font.Size:=fsize;
    FNormal.canvas.font.Style:=style;

    FNormal.canvas.font.color:=clGray;
    FNormal.Canvas.TextOut(st_x+1,st_y+1,title);
    FNormal.canvas.font.color:=clWhite;
    FNormal.Canvas.TextOut(st_x,st_y,title);

    FOver.LoadFromFile(dir+'/start_button_over.png');
    FOver.canvas.font.Size:=fsize;
    FOver.canvas.font.Style:=style;
    FOver.canvas.font.color:=clGray;
    FOver.Canvas.TextOut(st_x+1,st_y+1,title);
    FOver.canvas.font.color:=clWhite;
    FOver.Canvas.TextOut(st_x,st_y,title);

    FPressed.LoadFromFile(dir+'/start_button_press.png');
    FPressed.canvas.font.Size:=fsize;
    FPressed.canvas.font.Style:=style;
    FPressed.canvas.font.color:=clGray;
    FPressed.Canvas.TextOut(st_x+1,st_y+1,title);
    FPressed.canvas.font.color:=clWhite;
    FPressed.Canvas.TextOut(st_x,st_y,title);    

    bitmap.assign(FNormal);
    width:=bitmap.width;
    height:=bitmap.height;
end;

end.
