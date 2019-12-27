{ *************************************************************************** }
{                                                                             }
{ This file is part of the XPde project                                       }
{                                                                             }
{ Copyright (c) 2002 Jose Leon Serna <ttm@xpde.com>                           }
{                                                                             }
{ Portions translated from KDE C++ Code                                       }
{ Copyright (C) 1997 Matthias Kalle Dalheimer (kalle@kde.org)                 }
{ Copyright (c) 1998, 1999 KDE Team                                           }
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
unit uXPIPC;

interface

uses
    Classes, Sysutils, QForms, Qt, XLib, QDialogs;

const
    XPDE_BASE=3569;
    XPDE_DESKTOPCHANGED=XPDE_BASE+1;
    XPDE_ADDTRAYICON=XPDE_BASE+2;
    XPDE_REMOVETRAYICON=XPDE_BASE+3;

type
    //To notify about communications
    TIPCNotification=procedure(Sender:TObject; msg:integer; data: integer) of object;

    //Class for InterProcess Communications
    TXPIPC=class(TObject)
    private
        appAtom: Atom;
        FOnNotification: TIPCNotification;
        procedure enumWindows(w: window; msg, data: integer);
        function getSimpleProperty(w: window; a: atom): longint;
        procedure SetOnNotification(const Value: TIPCNotification);
    public
        procedure sendMessage(msg: integer; w: window; data: integer);
        procedure broadcastMessage(msg:integer; data:integer);
        procedure setupApplication;
        constructor Create;
        property OnNotification: TIPCNotification read FOnNotification write SetOnNotification;
    end;

var
    XPIPC:TXPIPC=nil;

implementation

var
    oldEventFilter: X11EventFilter;
    notificationAtom: Atom;

//Sends the message to all XPde applications
procedure TXPIPC.broadcastMessage(msg, data: integer);
begin
    enumWindows(XRootWindow(application.Display,XDefaultScreen(application.Display)),msg,data);
end;

constructor TXPIPC.Create;
begin
    //Setup the atoms we are going to need
    appAtom:=XInternAtom(application.display, 'XPDE_APLICATION', 0);
    notificationAtom:=XInternAtom(application.display, 'XPDE_NOTIFICATION', 0);
end;

//Returns the value of a window property, if exists
function TXPIPC.getSimpleProperty(w:window;a:atom):longint;
var
    real_type: Atom;
    format: integer;
    n,extra,res: integer;
    p: array [0..255] of integer;
    status:integer;
begin
    res := 0;
    status := XGetWindowProperty(application.display, w, a, 0, 1, 0, a,  @real_type, @format,  @n, @extra, @p);
    if (status = Success) then begin
        if ((n = 1) and (format = 32))  then begin
            res := p[0];
        end;
    end;
    result:=res;
end;

//Sends a message to a window
procedure TXPIPC.sendMessage(msg: integer; w: window; data: integer);
var
    ev: XEvent;
begin
    ev.xclient.xtype := ClientMessage;
    ev.xclient.display := application.display;
    ev.xclient.xwindow := w;
    ev.xclient.message_type := notificationAtom;
    ev.xclient.format := 32;
    ev.xclient.data.l[0] := msg;
    ev.xclient.data.l[1] := data;
    XSendEvent(application.display, w, 0, 0, @ev);
end;

//Checks recursively for XPde applications to send the message
procedure TXPIPC.enumWindows(w: window; msg:integer; data:integer);
var
    i: integer;
    root_win, parent_win: window;
    num_children: integer;
    i_list,child_list: PWindow;
begin
    child_list:=nil;
    root_win:=0;
    parent_win:=0;
    num_children:=0;

    //If it's an XPde app, then sends the message
    if getSimpleProperty(w,appAtom)<>0 then sendMessage(msg,w,data)
    else begin
        //Checks the children of that window
        if (XQueryTree(application.display, w, @root_win, @parent_win, @child_list, @num_children)=0) then begin
            writeln('Can''t query window tree. '+inttohex(w,8));
            exit;
        end;

        //Calls recursively
        i_list:=child_list;
        for i:=num_children-1 downto 0 do begin
            enumwindows(child_list^, msg, data);
            inc(child_list);
        end;

        if (i_list<>nil) then XFree(i_list);
    end;

end;

function eventhandler(event: PXEvent):boolean;cdecl;
begin
    if event.xtype=ClientMessage then begin
        if event.xclient.message_type=notificationatom then begin
            if assigned(XPIPC.OnNotification) then begin
                XPIPC.OnNotification(XPIPC,event^.xclient.data.l[0], event^.xclient.data.l[1]);
                result:=true;
            end
            else result:=false;
        end
        else begin
            if assigned(oldeventfilter) then result:=oldeventfilter(event)
            else result:=false;
        end;
    end
    else begin
        if assigned(oldeventfilter) then result:=oldeventfilter(event)
        else result:=false;

    end;
end;

procedure TXPIPC.setupApplication;
var
    w: window;
    data: integer;
begin
    //Setups the event filter to process the clientmessages
    oldeventfilter:=application.SetX11EventFilter(eventHandler);

    //Sets this app to receive XPDE_NOTIFICATIONS
    w:=QWidget_winId(application.appwidget);
    data := 1;
    XChangeProperty(application.display, w, appAtom, appAtom, 32, PropModeReplace, @data, 1);
end;

procedure TXPIPC.SetOnNotification(const Value: TIPCNotification);
begin
    FOnNotification := Value;
end;

initialization
    XPIPC:=TXPIPC.Create;

end.
