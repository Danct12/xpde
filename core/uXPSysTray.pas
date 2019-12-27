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
unit uXPSysTray;

interface

uses
    Classes, QExtCtrls, uCommon,
    QControls, QStdCtrls, SysUtils,
    QGraphics;

type
    TXPSysTray=class(TPanel)
    private
        separator: TPanel;
        client: TPanel;
        clock: TLabel;
        timer: TTimer;
    public
        procedure updatewidth;
        procedure SetParent(const Value: TWidgetControl); override;
        procedure setup;
        procedure updateclock(sender:TObject);
        constructor Create(AOwner:TComponent);override;
        destructor Destroy;override;
    end;

implementation

{ TXPSysTray }

constructor TXPSysTray.Create(AOwner: TComponent);
begin
  inherited;
  setup;
end;

destructor TXPSysTray.Destroy;
begin
  separator.free;
  clock.free;
  timer.free;
  client.free;
  inherited;
end;

procedure TXPSysTray.SetParent(const Value: TWidgetControl);
begin
  inherited;
  { TODO : Colors must be taken from the theme }
  clock.Font.color:=clWhite;
end;

procedure TXPSysTray.setup;
var
    dir: string;
begin
    BevelOuter:=bvNone;
    dir:=getSystemInfo(XP_TASKBAR_DIR);
    { TODO : Allow add tray icons, configure width according to clock, icons and height }

    width:=100; //Just temp
    separator:=TPanel.create(nil);
    separator.bevelouter:=bvNone;
    separator.bitmap.loadfromfile(dir+'/taskbar_separator.png');
    separator.width:=separator.Bitmap.Width;
    separator.align:=alLeft;
    separator.parent:=self;

    client:=TPanel.create(nil);
    client.BevelOuter:=bvNone;
    { TODO : This probably will be another background }
    client.bitmap.LoadFromFile(dir+'/taskbar_background_bottom.png');
    client.align:=alClient;
    client.parent:=self;

    clock:=TLabel.create(nil);
    clock.align:=alRight;
    clock.transparent:=true;
    clock.autosize:=true;
    clock.Layout:=tlCenter;
    clock.parent:=client;
    updateclock(nil);

    timer:=TTimer.create(nil);
    timer.Interval:=20000;
    timer.OnTimer:=updateclock;

    updatewidth;

end;


procedure TXPSysTray.updateclock(sender:TObject);
begin
    clock.Caption:=formatdatetime('  hh:nn  ',now());
end;

procedure TXPSysTray.updatewidth;
begin
    width:=clock.width+separator.width;
end;

end.
