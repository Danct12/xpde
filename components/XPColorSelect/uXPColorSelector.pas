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
unit uXPColorSelector;

interface

uses
  SysUtils, Types, Classes,
  QGraphics, QControls, QForms,
  QDialogs, uXPColorDialog, uGraphics;

type
  TXPColorSelector = class(TGraphicControl)
  private
    FColor: TColor;
    FDown: boolean;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetDown(const Value: boolean);
    procedure SetOnChange(const Value: TNotifyEvent);
    { Private declarations }
  protected
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    { Public declarations }
    procedure Paint;override;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
  published
    { Published declarations }
    property Color: TColor read FColor write SetColor;
    property Down: boolean read FDown write SetDown;
    property OnChange:TNotifyEvent read FOnChange write SetOnChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('XPde', [TXPColorSelector]);
end;



{ TXPColorSelector }

constructor TXPColorSelector.Create(AOwner: TComponent);
begin
  inherited;
  FColor:=clNone;
  width:=75;
  height:=21;
end;

destructor TXPColorSelector.Destroy;
begin
  inherited;

end;

procedure TXPColorSelector.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  Down:=true;
  SetCaptureControl(self);
end;

procedure TXPColorSelector.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
    p: TPoint;
    mr: TModalResult;
begin
  inherited;
  Down:=false;
  SetCaptureControl(nil);
  with TXPColorDialog.create(application) do begin
    selectedColor:=FColor;
    p.x:=self.boundsrect.left;
    p.y:=self.boundsrect.bottom;
    p:=self.parent.ClientToScreen(p);
    if (p.y+height)>screen.height then begin
        p.x:=self.boundsrect.left;
        p.y:=self.BoundsRect.Top;
        p:=self.parent.ClientToScreen(p);
        p.y:=p.y-height;
    end;
    if (p.x+width)>screen.width then begin
        p.x:=screen.width-width;
    end;
    left:=p.x;
    top:=p.y;
    try
        mr:=showmodal;
        if mr=mrOk then self.Color:=selectedcolor;
        if mr=mrAll then begin
            with TColorDialog.create(application) do begin
                try
                    color:=self.color;
                    if execute then begin
                        self.color:=color;
                    end;
                finally
                    free;
                end;
            end;
        end;
    finally
        free;
    end;
  end;
end;

procedure TXPColorSelector.Paint;
var
    x,y:integer;
    d: integer;
begin
    canvas.brush.color:=clButton;
    r3d(canvas,clientrect,false,not FDown,true);

    d:=0;
    if FDown then d:=1;
    canvas.pen.color:=clBlack;
    canvas.brush.color:=FColor;
    canvas.rectangle(rect(4+d,4+d,clientrect.right-15+d,clientrect.bottom-4+d));
    canvas.pen.color:=clBtnShadow;
    canvas.moveto(clientrect.right-12+d,4+d);
    canvas.lineto(clientrect.right-12+d,clientrect.bottom-5+d);
    canvas.pen.color:=clBtnHighLight;
    canvas.moveto(clientrect.right-11+d,4+d);
    canvas.lineto(clientrect.right-11+d,clientrect.bottom-5+d);

    x:=(clientrect.right-9)+d;
    y:=((height-3) div 2)+d;

    canvas.Pen.Color:=clBlack;
    with Canvas do begin
        moveto(x,y);
        lineto(x+4,y);
        moveto(x+1,y+1);
        lineto(x+3,y+1);
        moveto(x+2,y+2);
        lineto(x+2,y+2);
    end;

end;

procedure TXPColorSelector.SetColor(const Value: TColor);
begin
    if FColor<>Value then begin
        FColor := Value;
        paint;
        if assigned(FonChange) then FOnChange(self);
    end;
end;

procedure TXPColorSelector.SetDown(const Value: boolean);
begin
    if FDown<>Value then begin
        FDown := Value;
        paint;
    end;
end;

procedure TXPColorSelector.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

end.
 