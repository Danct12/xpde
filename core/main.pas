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
unit main;

interface

uses
  SysUtils, Types, Classes,
  Variants, QTypes, QGraphics,
  QControls, QForms, QDialogs,
  QStdCtrls, uXPDesktop, QButtons,
  uCommon, QMenus, Libc;

type
  TMainform = class(TForm)
    desktop_properties: TPopupMenu;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Properties1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Properties1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure createDesktop;
  end;

var
  Mainform: TMainform;
  desktop: TXPDesktop;

implementation

{$R *.xfm}

//Creates the desktop object
procedure TMainform.createDesktop;
begin
    desktop:=TXPDesktop.create(self);
    desktop.Font.assign(self.font);
    desktop.parent:=self;
    desktop.font.color:=clWhite;
    desktop.align:=alClient;
    desktop.popupmenu:=desktop_properties;
    application.Font.assign(self.font);
    desktop.setup;
end;

procedure TMainform.FormCreate(Sender: TObject);
begin
    //Stores the app dir and creates the desktop
    storeAppDir;
    BorderStyle:=fbsNone;
    left:=0;
    top:=0;
    width:=screen.width;
    height:=screen.height;
    createDesktop;
    desktop.SendToBack;

end;

procedure TMainform.Exit1Click(Sender: TObject);
begin
    application.terminate;
end;

procedure TMainform.Properties1Click(Sender: TObject);
var
    applet: string;
begin
    //Executes the properties applet
    { TODO : Develop a common way to execute apps }
    applet:=getSystemInfo(XP_APP_DIR)+'applets/desktop_properties &';
    libc.system(PChar(applet));
end;

procedure TMainform.FormShow(Sender: TObject);
begin
    sendtoback;
end;

end.
