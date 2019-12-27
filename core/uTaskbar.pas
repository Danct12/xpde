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
unit uTaskbar;

interface

uses
  SysUtils, Types, Classes,
  Variants, QTypes, QGraphics,
  QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, XLib,
  uWMConsts, uXPTaskbar;

type
  TTaskbar = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure startbuttonDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
        ataskbar: TXPTaskbar;
    { Public declarations }
        procedure addwindowtotray(const w: window);
        procedure removewindowfromtray(const w: window);
        procedure addtask(const client:IWMClient);
        procedure updatetask(const task:IWMClient);
        procedure activatetask(const task:IWMClient);
        procedure removetask(const task:IWMClient);
        function getRect:TRect;
  end;

var
  taskbar: Ttaskbar;

implementation

{ TODO : Use theme directories for the several images that make up the taskbar }

{$R *.xfm}

procedure Ttaskbar.FormCreate(Sender: TObject);
begin
    height:=29;
    left:=0;
    top:=qforms.screen.Height-height;
    width:=qforms.screen.width;
    ataskbar:=TXPTaskbar.create(self);
    ataskbar.align:=alClient;
    ataskbar.Parent:=self;
end;

procedure Ttaskbar.startbuttonDblClick(Sender: TObject);
begin
    application.Terminate;
end;

procedure TTaskbar.activatetask(const task: IWMClient);
begin
    ataskbar.activatetask(task);
end;

procedure TTaskbar.addtask(const client: IWMClient);
begin
    ataskbar.addtask(client);
end;

procedure TTaskbar.addwindowtotray(const w: window);
begin

end;

function TTaskbar.getRect: TRect;
begin
    result:=boundsrect;
end;

procedure TTaskbar.removetask(const task: IWMClient);
begin
    ataskbar.removetask(task);
end;

procedure TTaskbar.removewindowfromtray(const w: window);
begin

end;

procedure TTaskbar.updatetask(const task: IWMClient);
begin
    ataskbar.updatetask(task);
end;

procedure TTaskbar.FormShow(Sender: TObject);
begin
    bringtofront;
end;

end.
