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
unit uXPDesktop;

interface

{ TODO : All this must use the XPFS, once is finished }

uses
    Classes, uXPShellListView, uRegistry,
    QGraphics, uCommon, SysUtils,
    QDialogs, uXPIPC;

type
    //Inherits from the shelllistview
    TXPDesktop=class(TXPShellListView)
    public
        procedure IPCNotification(Sender:TObject; msg:integer; data: integer);
        procedure setup;
        procedure configureBackground;
        function getSystemImage(Sender:TObject; const itemindex:integer):integer;
        function getSystemCaption(Sender:TObject; const itemindex:integer):string;
        constructor Create(AOwner:TComponent);override;
        destructor Destroy;override;
    end;

implementation

{ TXPDesktop }

//Configures the background
procedure TXPDesktop.configureBackground;
var
    registry: TRegistry;
    bImage: string;
    method: integer;
    color: TColor;
    wallpapers_dir: string;
begin
    //Read configurations from the registry
    registry:=TRegistry.Create;
    try
        registry.RootKey:=HKEY_CURRENT_USER;
        if registry.OpenKey('Software/XPde/Desktop/Background', false) then begin
            bImage:=registry.ReadString('Image');
            method:=registry.ReadInteger('Method');
            color:=stringtocolor(registry.ReadString('Color'));
            if (bImage<>'') and (bImage<>'none') then begin
                self.color:=color;
                setbackgroundimage(bImage,method);
            end
            else setbackgroundcolor(color);
        end
        else begin
            wallpapers_dir:=getSystemInfo(XP_WALLPAPERS_DIR);
            if (fileexists(wallpapers_dir+'/default.png')) then begin
                setbackgroundimage(wallpapers_dir+'/default.png',2);
            end
            else setbackgroundcolor(clHighLight);
        end;
    finally
        registry.free;
    end;
end;

constructor TXPDesktop.Create(AOwner: TComponent);
begin
  inherited;
  //Setups the IPC component to receive notifications from changes on the desktop configuration
  XPIPC.setupApplication;
  XPIPC.OnNotification:=IPCNotification;
end;

destructor TXPDesktop.Destroy;
begin
  inherited;
end;

function TXPDesktop.getSystemCaption(Sender: TObject;
  const itemindex: integer): string;
begin
    //Returns a system caption
    case itemindex of
        0: result:='My Documents';
        1: result:='My Computer';
        2: result:='My Home';
        3: result:='My Network Places';
        4: result:='Recycle Bin';
    end;
end;

function TXPDesktop.getSystemImage(Sender: TObject;
  const itemindex: integer): integer;
begin
    //Returns a system image
    result:=itemindex+2;
end;

//If the desktop must be refreshed
procedure TXPDesktop.IPCNotification(Sender: TObject; msg, data: integer);
begin
    case msg of
        XPDE_DESKTOPCHANGED: begin
            configurebackground;
            redraw();
        end;
    end;
end;

procedure TXPDesktop.setup;
var
    desktop_dir: string;
begin
    IconLocationsKey:='Software/XPde/Desktop/IconLocations';

    //Configures the background
    configureBackground;

    OnGetSystemImage:=getSystemimage;
    OnGetSystemCaption:=getSystemCaption;

    //Sets the desktop directory
    SystemIcons:=5;
    desktop_dir:=getSystemInfo(XP_DESKTOP_DIR);
    forcedirectories(desktop_dir);
    Directory:=desktop_dir;
end;

end.
