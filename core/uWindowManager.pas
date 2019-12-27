{ *************************************************************************** }
{                                                                             }
{ This file is part of the XPde project                                       }
{                                                                             }
{ Copyright (c) 2002 Jose Leon Serna <ttm@xpde.com>                           }
{                                                                             }
{ Portions translated from Metacity Copyright (c) Havoc Pennington            }
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

unit uWindowManager;

interface

uses QForms, SysUtils, Types,
     QGraphics, Classes, Qt,
     Libc, XLib, Xpm,
     uActiveTasks, uWMConsts,
     QDialogs, QMenus, uCommon;

type
    TWMClient=class;

    IWMFrame=interface
    ['{3CC8B247-43EF-D611-88C2-000244219999}']
        function getFrameBorderSizes:TRect;                                     //Must return the sizes of the borders of the frame
        function getOrigin:TPoint;                                        //Must return the point where the client window is going to be placed
        procedure setClient(AClient:TWMClient);                                 //Must store the client to perform operations with the window
        procedure setTitle(ATitle:widestring);
        function getTitle:widestring;
        procedure updateActiveState;
    end;

    TXPWindowManager=class(TObject)
    private
        FServerGrabCount: integer;
        FDisplay: PDisplay;
        FScreen: integer;
        FRoot: Window;
        FClients: TList;
        cList: TList;
        FAtoms: array [0..maxAtoms] of Atom;
        FFrame: TFormClass;
        FActiveClient: TWMClient;
        procedure setupSignals;
        procedure setupEventHandler;
        procedure setupErrorHandler;
        procedure setupAtoms;
        procedure createExistingWindows;
        procedure setupKeyboardGrab;
        procedure setupDisplay;
        function set_wm_icon_size_hint: integer;
        function set_wm_check_hint: integer;

        function GetAtoms(Index: Integer): Atom;
        procedure SetAtoms(Index: Integer; const Value: Atom);
        procedure setFrame(AFrameClass: TFormClass);
        procedure SetActiveClient(const Value: TWMClient);
        function getFramedClients: TList;
    public
        function hasborder(xwindow:window):boolean;
        procedure bypasswindow(xwindow:window);
        procedure grabDisplay;
        procedure ungrabDisplay;
        procedure install;
        function getDesktopClientRect:TRect;
        function findClient(w: Window):TWMClient;
        procedure sendalltoback(exclude:TList);
        function findTransientFor(transient: Window):TWMClient;
        function createNewClient(w:Window):TWMClient;
        procedure closeActiveWindow;
        //**********************************
        function handleButtonPress(var event:XEvent):integer;
        function handleButtonRelease(var event:XEvent):integer;
        function handleKeyRelease(var event:XEvent):integer;
        function handleMotionNotify(var event:XEvent):integer;
        function handleEnterNotify(var event:XEvent):integer;
        function handleLeaveNotify(var event:XEvent):integer;
        function handleFocusInOut(var event:XEvent):integer;
        function handleDestroyNotify(var event:XEvent):integer;
        function handleUnmapNotify(var event:XEvent):integer;
        function handleMapRequest(var event:XEvent):integer;
        function handleConfigureNotify(var event:XEvent):integer;
        function handleConfigureRequest(var event:XEvent):integer;
        function handlePropertyNotify(var event:XEvent):integer;
        function handleSelectionClear(var event:XEvent):integer;
        function handleSelectionRequest(var event:XEvent):integer;
        function handleColorMapNotify(var event:XEvent):integer;
        function handleClientMessage(var event:XEvent):integer;
        function handleMappingNotify(var event:XEvent):integer;
        //**********************************
        property Atoms[Index: Integer]: Atom read GetAtoms write SetAtoms;
        property Clients:TList read FClients;
        property FramedClients: TList read getFramedClients;
        property Frame: TFormClass read FFrame write SetFrame;
        property Display: PDisplay read FDisplay write FDisplay;
        property Root: Window read FRoot write FRoot;
        property ActiveClient: TWMClient read FActiveClient write SetActiveClient;
        constructor Create;
        destructor Destroy;override;
    end;

    TWMClient=class(TInterfacedObject, IWMClient)
    private
        xwindow: Window;
        xtransientfor: window;
        frame: TForm;
        wRect: TRect;
        inresize: boolean;
        FUnmapCounter:integer;
        FWindowManager: TXPWindowManager;
        FWindowState: TWindowState;
        minimizedstate: TWindowState;
        framed: boolean;
        procedure SetWindowManager(const Value: TXPWindowManager);
        procedure SetWindowState(const Value: TWindowState);
    public
        procedure configure_request(event: XEvent);
        procedure gravitate;
        procedure reparent;
        procedure resize;
        procedure map;
        procedure focus;
        procedure updatetransientfor;
        procedure createFrame;
        function hasBorder:boolean;
        procedure getIcons;
        function isKDETray:boolean;
        procedure getSizeHints;
        procedure minimize;
        procedure maximize;
        procedure sendsyntheticConfigureNotify;
        procedure restore;
        procedure setMapState(state:integer);
        procedure getMapState;
        procedure beginresize;
        procedure endresize;
        procedure close;
        procedure bringtofront;
        procedure updateactivewindow;
        procedure sendtoback;
        function getWindow: Window;
        procedure updateactivestate;
        function isactive:boolean;
        function isframed: boolean;
        procedure activate(restore:boolean=true);
        function getTitle: widestring;
        property WindowManager:TXPWindowManager read FWindowManager write SetWindowManager;
        property UnmapCounter: integer read FUnmapCounter write FUnmapCounter;
        property Wnd: Window read xwindow write xwindow;
        property WindowState: TWindowState read FWindowState write SetWindowState;
        constructor Create(AWindow:Window; AWindowManager:TXPWindowManager);
        function getBitmap: TBitmap;
        function getSystemMenu: TPopupMenu;
        function getIcon: TBitmap;
        destructor Destroy;override;
    end;

    TXLibInterface=class(TObject)
    private
    public
        indent: integer;
        function formatWindow(w:Window):string;
        procedure outputDebugString(const kind:integer;const str:string);
        constructor Create;
        function GetWindowProperty(xdisplay: PDisplay; xwindow: Window; xatom, req_type: Atom): variant;
        function getSimpleProperty(w: window; a: atom): longint;
    end;


    TQX11EventFilter=function(var ev:XEvent):integer;cdecl;

const
    iMETHOD=1;
    iINFO=2;
    iWARNING=3;
    iERROR=4;
    iBEGINPROCESS=5;
    iENDPROCESS=6;
    iEVENT=7;

    QtShareName = '';

    ChildMask=(SubstructureRedirectMask or SubstructureNotifyMask);
    ButtonMask=(ButtonPressMask or ButtonReleaseMask);
    MouseMask=(ButtonMask or PointerMotionMask);


//function qt_set_x11_event_filter(filter:TQX11EventFilter):TQX11EventFilter; cdecl;

var
    XLibInterface: TXLibInterface;
    XPWindowManager: TXPWindowManager;
    oldevent: X11EventFilter;

implementation

uses uXPStyledFrame, uTaskbar, uXPStartMenu;

//function qt_set_x11_event_filter; external QtShareName name 'qt_set_x11_event_filter__FPFP7_XEvent_i';


//******************************************************************************
procedure handleSignal(signo:integer);
begin
    showmessage(inttostr(signo));
end;

function event_detail_to_string (d:integer):string;
var
    detail: string;
begin
  detail := '???';
  case (d) of
    // We are an ancestor in the A<->B focus change relationship
    NotifyAncestor: detail := 'NotifyAncestor';
    NotifyDetailNone: detail := 'NotifyDetailNone';
    // We are a descendant in the A<->B focus change relationship
    NotifyInferior: detail := 'NotifyInferior';
    NotifyNonlinear: detail := 'NotifyNonlinear';
    NotifyNonlinearVirtual: detail := 'NotifyNonlinearVirtual';
    NotifyPointer: detail := 'NotifyPointer';
    NotifyPointerRoot: detail := 'NotifyPointerRoot';
    NotifyVirtual: detail := 'NotifyVirtual';
  end;
  result:=detail;
end;


function event_mode_to_string (m: integer):string;
var
    mode: string;
begin
    mode:='???';
    case (m) of
        NotifyNormal: mode := 'NotifyNormal';
        NotifyGrab: mode := 'NotifyGrab';
        NotifyUngrab: mode := 'NotifyUngrab';
        NotifyWhileGrabbed: mode := 'NotifyWhileGrabbed';
    end;
    result:=mode;
end;

function stack_mode_to_string(mode:integer):string;
begin
  case (mode) of
    Above: result:='Above';
    Below: result:='Below';
    TopIf: result:='TopIf';
    BottomIf: result:='BottomIf';
    Opposite: result:='Opposite';
    else result:='Unknown';
  end;
end;

function key_event_description (xdisplay: PDisplay; var event: XEvent):string;
var
  key:KeySym;
  str: string;
begin
  key := XKeycodeToKeysym (xdisplay, event.xkey.keycode, 0);

  str := XKeysymToString (key);

  if (trim(str)='') then str:='(nil)';

  result:=format('Key ''%s'' state $%x',[str,event.xkey.state]);

end;


procedure spewEvent(xdisplay:PDisplay;var event: XEvent);
var
    name: string;
    extra: string;
//    winname: string;
    str: PChar;
    state: string;
    s1,s2,s3,s4,s5,s6,s7: string;
begin
  // filter overnumerous events
  if (event.xtype = Expose) or  (event.xtype = MotionNotify) or (event.xtype = NoExpose) then exit;

  case (event.xtype) of
    KeyPress: begin
      name := 'KeyPress';
      extra := key_event_description (xdisplay, event);
    end;
    KeyRelease: begin
      name := 'KeyRelease';
      extra := key_event_description (xdisplay, event);
    end;
    ButtonPress: begin
      name := 'ButtonPress';
      extra := format ('button %d state 0x%x x %d y %d root 0x%x same_screen %d',
                              [event.xbutton.button,
                               event.xbutton.state,
                               event.xbutton.x,
                               event.xbutton.y,
                               event.xbutton.root,
                               event.xbutton.same_screen]);
    end;
    ButtonRelease: begin
      name := 'ButtonRelease';
      extra := format('button %d state 0x%x x %d y %d root 0x%x same_screen %d',
                               [event.xbutton.button,
                               event.xbutton.state,
                               event.xbutton.x,
                               event.xbutton.y,
                               event.xbutton.root,
                               event.xbutton.same_screen]);
    end;
    MotionNotify: begin
      name := 'MotionNotify';
      extra := format ('win: 0x%x x: %d y: %d',
                               [event.xmotion.xwindow,
                               event.xmotion.x,
                               event.xmotion.y]);
    end;
    EnterNotify: begin
      name := 'EnterNotify';
      extra := format ('win: 0x%x root: 0x%x subwindow: 0x%x mode: %s detail: %s focus: %d x: %d y: %d',[
                               event.xcrossing.xwindow,
                               event.xcrossing.root,
                               event.xcrossing.subwindow,
                               event_mode_to_string (event.xcrossing.mode),
                               event_detail_to_string (event.xcrossing.detail),
                               event.xcrossing.focus,
                               event.xcrossing.x,
                               event.xcrossing.y]);
    end;
    LeaveNotify: begin
      name := 'LeaveNotify';
      extra := format ('win: 0x%x root: 0x%x subwindow: 0x%x mode: %s detail: %s focus: %d x: %d y: %d',[
                               event.xcrossing.xwindow,
                               event.xcrossing.root,
                               event.xcrossing.subwindow,
                               event_mode_to_string (event.xcrossing.mode),
                               event_detail_to_string (event.xcrossing.detail),
                               event.xcrossing.focus,
                               event.xcrossing.x,
                               event.xcrossing.y]);
    end;
    FocusIn: begin
      name := 'FocusIn';
      extra := format ('detail: %s mode: %s\n',[
                               event_detail_to_string (event.xfocus.detail),
                               event_mode_to_string (event.xfocus.mode)]);
    end;
    FocusOut: begin
      name := 'FocusOut';
      extra := format ('detail: %s mode: %s\n',[
                               event_detail_to_string (event.xfocus.detail),
                               event_mode_to_string (event.xfocus.mode)]);
    end;
    KeymapNotify: begin
      name := 'KeymapNotify';
    end;
    Expose: begin
      name := 'Expose';
    end;
    GraphicsExpose: begin
      name := 'GraphicsExpose';
    end;
    NoExpose: begin
      name := 'NoExpose';
    end;
     VisibilityNotify: begin
      name := 'VisibilityNotify';
      end;
     CreateNotify: begin
      name := 'CreateNotify';
      end;
     DestroyNotify: begin
      name := 'DestroyNotify';
      extra := format ('event: 0x%x window: 0x%x ',[
                               event.xdestroywindow.event,
                               event.xdestroywindow.xwindow]);
      end;
     UnmapNotify: begin
      name := 'UnmapNotify';
      extra := format ('event: 0x%x window: 0x%x from_configure: %d',[
                               event.xunmap.event,
                               event.xunmap.xwindow,
                               event.xunmap.from_configure]);
      end;
     MapNotify: begin
      name := 'MapNotify';
      extra := format ('event: 0x%x window: 0x%x override_redirect: %d',[
                               event.xmap.event,
                               event.xmap.xwindow,
                               event.xmap.override_redirect]);
      end;
     MapRequest: begin
      name := 'MapRequest';
      end;
     ReparentNotify: begin
      name := 'ReparentNotify';
      extra := format ('event: 0x%x window: 0x%x parent: 0x%x ',[
                               event.xreparent.event,
                               event.xreparent.xwindow,
                               event.xreparent.parent]);
      end;
     ConfigureNotify: begin
      name := 'ConfigureNotify';
      extra := format ('x: %d y: %d w: %d h: %d above: 0x%x override_redirect: %d',[
                               event.xconfigure.x,
                               event.xconfigure.y,
                               event.xconfigure.width,
                               event.xconfigure.height,
                               event.xconfigure.above,
                               event.xconfigure.override_redirect]);
      end;
     ConfigureRequest: begin
     s1:='(unset)';
     s2:=s1;
     s3:=s1;
     s4:=s1;
     s5:=s1;
     s6:=s1;
     s7:=s1;
     if ((event.xconfigurerequest.value_mask and CWX)=CWX) then s1:='';
     if ((event.xconfigurerequest.value_mask and CWY)=CWY) then s2:='';
     if ((event.xconfigurerequest.value_mask and CWWidth)=CWWidth) then s3:='';
     if ((event.xconfigurerequest.value_mask and CWHeight)=CWHeight) then s4:='';
     if ((event.xconfigurerequest.value_mask and CWBorderWidth)=CWBorderWidth) then s5:='';
     if ((event.xconfigurerequest.value_mask and CWSibling)=CWSibling) then s6:='';
     if ((event.xconfigurerequest.value_mask and CWStackMode)=CWStackMode) then s7:='';

      name := 'ConfigureRequest';
      extra := format ('parent: 0x%x window: 0x%x x: %d %sy: %d %sw: %d %sh: %d %sborder: %d %sabove: %x %sstackmode: %s %s',[
                               event.xconfigurerequest.parent,
                               event.xconfigurerequest.xwindow,
                               event.xconfigurerequest.x, s1,
                               event.xconfigurerequest.y, s2,
                               event.xconfigurerequest.width, s3,
                               event.xconfigurerequest.height, s4,
                               event.xconfigurerequest.border_width, s5,
                               event.xconfigurerequest.above, s6,
                               stack_mode_to_string (event.xconfigurerequest.detail), s7]);
      end;
     GravityNotify: begin
      name := 'GravityNotify';
      end;
     ResizeRequest: begin
      name := 'ResizeRequest';
      extra := format ('width := %d height := %d',[
                               event.xresizerequest.width,
                               event.xresizerequest.height]);
      end;
     CirculateNotify: begin
      name := 'CirculateNotify';
      end;
     CirculateRequest: begin
      name := 'CirculateRequest';
      end;
     PropertyNotify: begin
        name := 'PropertyNotify';

        str := XGetAtomName (xdisplay, event.xproperty.atom);

        if (event.xproperty.state = PropertyNewValue) then state := 'PropertyNewValue'
        else if (event.xproperty.state = PropertyDelete) then state := 'PropertyDelete'
        else state := '???';

        if assigned(str) then begin
            extra := format ('atom: %s state: %s', [str,state]);
        end
        else begin
            extra := format ('atom: unknown state: %s', [state]);
        end;

        XFree(str);
      end;
     SelectionClear: begin
      name := 'SelectionClear';
      end;
     SelectionRequest: begin
      name := 'SelectionRequest';
      end;
     SelectionNotify: begin
      name := 'SelectionNotify';
      end;
     ColormapNotify: begin
      name := 'ColormapNotify';
      end;
     ClientMessage: begin
     {
        char *str;
        name := 'ClientMessage';
        meta_error_trap_push (display);
        str := XGetAtomName (display.xdisplay,
                            event.xclient.message_type);
        meta_error_trap_pop (display, TRUE);
        extra := format ('type: %s format: %d\n',
                                 str ? str : '(unknown atom)',
                                 event.xclient.format);
        meta_XFree (str);
      }
      end;
     MappingNotify: begin
      name := 'MappingNotify';
      end;
    else begin
      name := '(Unknown event)';
      extra := format ('type: %d', [event.xany.xtype]);
      end;
  end;

{
  screen := meta_display_screen_for_root (display, event.xany.xwindow);

  if (screen)
    winname := format ('root %d', screen.number);
  else
    winname := format ('0x%x', event.xany.xwindow);

  meta_topic (META_DEBUG_EVENTS,
              '%s on %s%s %s %sserial %u\n', name, winname,
              extra ? ':' : '', extra ? extra : '',
              event.xany.send_event ? 'SEND ' : '',
              event.xany.serial);

  g_free (winname);

  if (extra)
    g_free (extra);
}
//    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iEVENT,name+' '+extra);
//    {$endif}
end;

function eventhandler(event: PXEvent):boolean;cdecl;
begin
    {$ifdef DEBUG}
    spewEvent(xpwindowmanager.display,event^);
    {$endif}
    case event.xtype of
        maprequest: begin
//            spewEvent(application.display,event^);
            result:=(xpwindowmanager.handleMapRequest(event^)=1);
        end;
        propertynotify: begin
            //spewEvent(application.display,event^);
            result:=(xpwindowmanager.handlePropertyNotify(event^)=1);
        end;
//        destroynotify: result:=(xpwindowmanager.handleDestroyNotify(event^)=1);
        unmapnotify: begin
//            spewEvent(application.display,event^);
            result:=(xpwindowmanager.handleUnmapNotify(event^)=1);
        end;
        ClientMessage: begin
//            spewEvent(application.display,event^);
            result:=(xpwindowmanager.handleClientMessage(event^)=1);
            if not result then begin
                if assigned(oldevent) then result:=oldevent(event)
                else result:=false;
            end;
        end;
        enternotify: begin
//            spewEvent(application.display,event^);
            result:=(xpwindowmanager.handleenternotify(event^)=1);
        end;
        buttonpress: begin
//            spewEvent(application.display,event^);
            result:=(xpwindowmanager.handlebuttonpress(event^)=1);
        end;
        {
        keypress: begin
            //showmessage('bbb');
            //result:=0;//result:=(xpwindowmanager.handlekeyrelease(event^)=1);
            //result:=true;
            //result:=false;
            result:=true;
        end;
        }
        keypress: begin
            if (event^.xkey.keycode=XKeysymToKeycode(qtdisplay, xk_tab)) then begin
                if not assigned(activetasksdlg) then begin
                    if xpwindowmanager.framedClients.Count>1 then begin
                        activetasksdlg:=TActiveTasksDlg.create(application);
                        activetasksdlg.show;
                        ActiveTasksDlg.SetFocus;
                    end;
                end
                else begin
                    ActiveTasksDlg.incActiveTask;
                end;
                result:=true;
            end
            else
            result:=false;
        end;
        keyrelease: begin
            //showmessage('aaa');
            //result:=0;//result:=(xpwindowmanager.handlekeyrelease(event^)=1);
            //result:=true;
            //result:=false;
            if (event^.xkey.keycode=XKeysymToKeycode(qtdisplay, xk_f4)) then begin
                XPWindowManager.closeactivewindow;
                result:=true;
            end
            {
            else if (event^.xkey.keycode=XKeysymToKeycode(qtdisplay, xk_tab)) then begin
                event^.xkey.state
                if assigned(activetasksdlg) then begin
                    activetasksdlg.free;
                    activetasksdlg:=nil;
                end;
                result:=true;
            end
            }
            else begin
                if assigned(oldevent) then result:=oldevent(event)
                else result:=false;
            end;
        end;
        configurerequest: begin
//            spewEvent(application.display,event^);
            result:=(xpwindowmanager.handleConfigurerequest(event^)=1);
        end;
        configurenotify: begin
//            spewEvent(application.display,event^);
            result:=(xpwindowmanager.handleConfigurenotify(event^)=1);
        end;
        else begin
        if assigned(oldevent) then result:=oldevent(event)
        else result:=false;
        end;
    end;
end;

function handleXerror(var dpy:Display;var e:XErrorEvent):integer;
begin
    result:=0;
end;

//******************************************************************************

{ TXPWindowManager }

constructor TXPWindowManager.Create;
begin
    inherited;
    clist:=TList.create;
    FActiveClient:=nil;
    FServerGrabCount:=0;
    FDisplay:=nil;
    FRoot:=none;
    FClients:=TList.create;
end;

procedure TXPWindowManager.createExistingWindows;
begin
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,'TXPWindowManager.createExistingWindows');
    {$endif}
end;

destructor TXPWindowManager.Destroy;
begin
    clist.free;
   FClients.free;
   inherited;
end;

function TXPWindowManager.GetAtoms(Index: Integer): Atom;
begin
    result:=FAtoms[Index];
end;

function TXPWindowManager.getDesktopClientRect: TRect;
var
    r: TRect;
    t: TRect;
begin
    r:=rect(0,0,qforms.screen.width,qforms.screen.height);
    t:=taskbar.getRect;
    //This needs more work, to know the real position of the taskbar
    //it only supports the taskbar at the bottom
    result:=rect(0,0,r.Right,t.top);
end;
// Grab/ungrab routines taken from fvwm
procedure TXPWindowManager.grabDisplay;
begin
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iBEGINPROCESS,'Grabdisplay');    
    xlibinterface.outputDebugString(iMETHOD,'TXPWindowManager.grabdisplay, FServerGrabCount='+inttostr(FServerGrabCount));
    {$endif}
  if (FServerGrabCount = 0) then begin
      XSync (FDisplay, 0);
      XGrabServer (FDisplay);
  end;
  XSync (FDisplay, 0);
  inc(FServerGrabCount);
end;

function TXPWindowManager.handleButtonPress(var event:XEvent): integer;
var
    c: TWMClient;
    xwindow: window;
    menu: TStartMenu;
//    name: XTextProperty;
//    attr: XWindowAttributes;
begin
    xwindow:=event.xany.xwindow;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,format('TXPWindowManager.handleButtonPress %s',[xlibinterface.formatwindow(xwindow)]));
    {$endif}

    menu:=getStartMenu;
//    XGetWMName(FDisplay, xwindow, @name);
//    XGetWindowAttributes(FDisplay, xwindow, @attr);
//    xlibinterface.outputDebugString(iINFO,format('*****attr (%d,%d) %dx%d',[attr.x,attr.y,attr.width,attr.height]));
//    writeln('press:'+inttostr(xwindow));
//    writeln('logoff:'+inttostr(QWidget_winId(menu.flogoff.handle)));
    if (XLibInterface.getSimpleProperty(xwindow,Atoms[atom_BY_PASS_WM])<>0) then begin
        //Do nothing!
    end
    else begin
        menu.Close;
    end;
    c:=findclient(xwindow);
    if assigned(c) then begin
//    writeln('press:'+inttostr(xwindow));
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,format('c.activate %s',[xlibinterface.formatwindow(xwindow)]));
    {$endif}
        c.activate;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,format('XAllowevents %s',[xlibinterface.formatwindow(xwindow)]));
    {$endif}
        XAllowEvents (Fdisplay, ReplayPointer, event.xbutton.time);
    end
    else begin
        {$ifdef DEBUG}
        xlibinterface.outputDebugString(iWARNING,format('Not client found to activate on BUTTONPRESS %s',[xlibinterface.formatwindow(xwindow)]));
        {$endif}
    end;
    result:=0;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,format('END TXPWindowManager.handleButtonPress %s',[xlibinterface.formatwindow(xwindow)]));
    {$endif}
end;

function TXPWindowManager.handleButtonRelease(var event:XEvent): integer;
begin
    result:=0;
end;

function TXPWindowManager.handleClientMessage(var event:XEvent): integer;
var
    c: TWMClient;
    xwindow: window;
begin
    result:=0;
    xwindow:=event.xclient.xwindow;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,format('TXPWindowManager.handleClientMessage %s',[xlibinterface.formatwindow(xwindow)]));
    {$endif}

    if ( event.xclient.message_type = atoms[atom_wm_change_state]) then begin
        c:=findclient(xwindow);
        if assigned(c) then begin
            if ( event.xclient.data.l[0] = IconicState) then begin
                c.minimize;
                result:=1;
            end;
        end;
    end;
    {
      gulong action;
      Atom first;
      Atom second;

      action = event->xclient.data.l[0];
      first = event->xclient.data.l[1];
      second = event->xclient.data.l[2];

    xlibinterface.outputDebugString(iMETHOD,format('TXPWindowManager.handleclient message',[]));
    }
end;

function TXPWindowManager.handleColorMapNotify(var event:XEvent): integer;
begin
    result:=0;
end;

function TXPWindowManager.handleConfigureNotify(var event:XEvent): integer;
var
    xwindow: window;
begin
    xwindow:=event.xconfigure.xwindow;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,format('TXPWindowManager.handleconfigurenotify %s',[xlibinterface.formatwindow(xwindow)]));
    {$endif}
    result:=1;
end;

function TXPWindowManager.handleConfigureRequest(var event:XEvent): integer;
var
    c: TWMClient;
    xwindow: window;
    xwcm: cardinal;
    xwc: XWindowChanges;
begin
      {* This comment and code is found in both twm and fvwm
       *
       * According to the July 27, 1988 ICCCM draft, we should ignore size and
       * position fields in the WM_NORMAL_HINTS property when we map a window.
       * Instead, we'll read the current geometry.  Therefore, we should respond
       * to configuration requests for windows which have never been mapped.
       *}
    xwindow:=event.xconfigurerequest.xwindow;

    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iBEGINPROCESS,'HANDLE_CONFIGURE_REQUEST');
    xlibinterface.outputDebugString(iMETHOD,format('TXPWindowManager.handleconfigurerequest %s',[xlibinterface.formatwindow(xwindow)]));
    {$endif}
    c:=findclient(xwindow);
    if (not(assigned(c))) then begin
        xwcm := event.xconfigurerequest.value_mask and (CWX or CWY or CWWidth or CWHeight or CWBorderWidth);

        xwc.x := event.xconfigurerequest.x;
        xwc.y := event.xconfigurerequest.y;
        xwc.width := event.xconfigurerequest.width;
        xwc.height := event.xconfigurerequest.height;
        xwc.border_width := event.xconfigurerequest.border_width;

        {$ifdef DEBUG}
        xlibinterface.outputDebugString(iINFO,format('Configuring withdrawn window %s to %d,%d %dx%d border %d (some values may not be in mask)',[xlibinterface.formatwindow(xwindow),xwc.x, xwc.y, xwc.width, xwc.height, xwc.border_width]));
        xlibinterface.outputDebugString(iINFO,format('%s to %d',[xlibinterface.formatwindow(event.xconfigurerequest.above),event.xconfigurerequest.detail]));
        {$endif}

        XConfigureWindow (Fdisplay, event.xconfigurerequest.xwindow, xwcm, @xwc);
    end
    else begin
        c.configure_request(event);
    end;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iENDPROCESS,'HANDLE_CONFIGURE_REQUEST');
    {$endif}
    result:=0;
end;

function TXPWindowManager.handleDestroyNotify(var event:XEvent): integer;
var
    c: TWMClient;
    p: TWMCLient;
    xwindow: window;
    i: integer;
begin
    xwindow:=event.xdestroywindow.xwindow;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iBEGINPROCESS,'HANDLE_DESTROY_NOTIFY');
    xlibinterface.outputDebugString(iMETHOD,'TXPWindowManager.handledestroyNotify '+xlibinterface.formatwindow(xwindow));
    {$endif}
    c:=findclient(xwindow);
    if assigned(c) then begin
        if (c.iskdetray) then begin
           {$ifdef DEBUG}
           xlibinterface.outputDebugString(iINFO,format('Removing tray client %s',[xlibinterface.formatwindow(xwindow)]));
           {$endif}
            clients.Remove(c);
            c.free;
        end;
    end
    else begin
        {$ifdef DEBUG}
        xlibinterface.outputDebugString(iINFO,'Client tray for window '+xlibinterface.formatwindow(xwindow)+' not found in handledestroyNotify');
        {$endif}
    end;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iENDPROCESS,'HANDLE_DESTROY_NOTIFY');
    {$endif}
    result:=0;
end;

function TXPWindowManager.handleEnterNotify(var event:XEvent): integer;
var
    c: TWMClient;
    xwindow: window;
begin
    xwindow:=event.xany.xwindow;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,format('TXPWindowManager.handleenternotify %s',[xlibinterface.formatwindow(xwindow)]));
    {$endif}
    c:=findclient(xwindow);
    if assigned(c) then begin
        if (c.isactive) then c.focus;
        result:=0;
    end
    else begin
        {$ifdef DEBUG}
        xlibinterface.outputDebugString(iWARNING,format('Not client found to focus %s',[xlibinterface.formatwindow(xwindow)]));
        {$endif}
        result:=1;
    end;
end;

function TXPWindowManager.handleFocusInOut(var event:XEvent): integer;
var
    c: TWMClient;
    xwindow: window;
begin
    xwindow:=event.xfocus.window;
    result:=0;
//    xlibinterface.outputDebugString(iMETHOD,format('TXPWindowManager.handlefocusinout %s',[xlibinterface.formatwindow(xwindow)]));
end;

function TXPWindowManager.handleLeaveNotify(var event:XEvent): integer;
begin
    result:=0;
end;

function TXPWindowManager.handleMappingNotify(var event:XEvent): integer;
begin
    xlibinterface.outputDebugString(iMETHOD,format('TXPWindowManager.handlemappingnotify',[]));
    result:=0;
end;

function TXPWindowManager.handleMapRequest(var event:XEvent): integer;
var
    c: TWMClient;
    xwindow: window;
begin
    xwindow:=event.xmaprequest.xwindow;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iBEGINPROCESS,'HANDLE_MAP_REQUEST');
    xlibinterface.outputDebugString(iMETHOD,format('TXPWindowManager.handlemaprequest %s',[xlibinterface.formatwindow(xwindow)]));
    {$endif}
    c:=findclient(xwindow);
    if assigned(c) then begin
//        xlibinterface.outputDebugString(iMETHOD,format('c.activate %s',[xlibinterface.formatwindow(xwindow)]));
        c.activate;
    end
    else begin
//        xlibinterface.outputDebugString(iMETHOD,format('createnewclient %s',[xlibinterface.formatwindow(xwindow)]));
        createNewClient(xwindow);
    end;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iENDPROCESS,'HANDLE_MAP_REQUEST');
    {$endif}
    result:=0;
end;

function TXPWindowManager.handleMotionNotify(var event:XEvent): integer;
begin
    result:=0;
end;

function TXPWindowManager.handlePropertyNotify(var event:XEvent): integer;
var
    c: TWMClient;
    xwindow: window;
    name: XTextProperty;
begin
    xwindow:=event.xproperty.xwindow;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,format('TXPWindowManager.handlePropertyNotify %s',[xlibinterface.formatwindow(xwindow)]));
    {$endif}
    if event.xproperty.atom=XA_WM_NAME then begin
        c:=findclient(xwindow);
        if assigned(c) then begin
            XGetWMName(FDisplay, xwindow, @name);
            if assigned(c.frame) then begin
                (c.frame as TStyledFrame).setTitle(listtostr(PChar(name.value)));
            end;
            taskBar.updatetask(c);
        end
        else begin
            {$ifdef DEBUG}
            xlibinterface.outputDebugString(iWARNING,format('Not client found to set new caption %s',[xlibinterface.formatwindow(xwindow)]));
            {$endif}
        end;
        result:=0;
    end
    else if (event.xproperty.atom = XA_WM_TRANSIENT_FOR) then begin
//      meta_verbose ("Property notify on %s for WM_TRANSIENT_FOR\n", window->desc);
        c:=findclient(xwindow);
        if assigned(c) then begin
            c.updatetransientfor;
        end;
//        update_transient_for (window);

//      meta_window_queue_move_resize (window);
    end
    else begin
        result:=1;
    end;
end;

function TXPWindowManager.handleSelectionClear(var event:XEvent): integer;
begin
    result:=0;
end;

function TXPWindowManager.handleSelectionRequest(var event:XEvent): integer;
begin
    result:=0;
end;

function TXPWindowManager.handleUnmapNotify(var event:XEvent): integer;
var
    c: TWMClient;
    p: TWMCLient;
    xwindow: window;
    i: integer;
begin
    xwindow:=event.xunmap.xwindow;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iBEGINPROCESS,'HANDLE_UNMAP_NOTIFY');
    xlibinterface.outputDebugString(iMETHOD,'TXPWindowManager.handleunmapNotify '+xlibinterface.formatwindow(xwindow));
    {$endif}
    c:=findclient(xwindow);
    if assigned(c) then begin
        {$ifdef DEBUG}
        xlibinterface.outputDebugString(iINFO,format('Checking client unmap counter: %d for window %s',[c.unmapcounter,xlibinterface.formatwindow(xwindow)]));
        {$endif}
        if (c.UnmapCounter=0) then begin
           {$ifdef DEBUG}
           xlibinterface.outputDebugString(iINFO,format('Removing client %s',[xlibinterface.formatwindow(xwindow)]));
           {$endif}
            clients.Remove(c);

            if c=FActiveClient then begin
                if Clients.count>=1 then begin
                    p:=clients[0];
                    if assigned(p) then begin
                        p.activate(false);
                    end
                    else ActiveClient:=nil;

                end
                else ActiveClient:=nil;
            end;
            c.free;
        end
        else c.UnmapCounter:=c.UnmapCounter-1;
    end
    else begin
        {$ifdef DEBUG}
        xlibinterface.outputDebugString(iINFO,'Client for window '+xlibinterface.formatwindow(xwindow)+' not found in handleUnmapNotify');
        {$endif}
    end;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iENDPROCESS,'HANDLE_UNMAP_NOTIFY');
    {$endif}
    result:=0;
end;

procedure TXPWindowManager.install;
begin
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iBEGINPROCESS,'WINDOW_MANAGER_INSTALLATION');
    xlibinterface.outputDebugString(iMETHOD, 'TXPWindowManager.install');
    {$endif}

    FDisplay := application.Display;
    setupErrorHandler;
    setupSignals;
    setupKeyboardGrab;
    setupAtoms;
    setupDisplay;
    createExistingWindows;
    setupEventHandler;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iENDPROCESS,'WINDOW_MANAGER_INSTALLATION');
    {$endif}
end;

procedure TXPWindowManager.SetAtoms(Index: Integer; const Value: Atom);
begin

end;

procedure TXPWindowManager.setupAtoms;
var
    i: integer;
    atomn: array[0..maxAtoms] of PChar;
    atomNames: TStringList;
begin
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,'TXPWindowManager.setupAtoms');
    {$endif}
    atomNames:=TStringList.create;
    try
        with atomNames do begin
            add('_NET_WM_NAME');
            add('WM_PROTOCOLS');
            add('WM_TAKE_FOCUS');
            add('WM_DELETE_WINDOW');
            add('WM_STATE');
            add('_NET_CLOSE_WINDOW');
            add('_NET_WM_STATE');
            add('_MOTIF_WM_HINTS');
            add('_NET_WM_STATE_SHADED');
            add('_NET_WM_STATE_MAXIMIZED_HORZ');
            add('_NET_WM_STATE_MAXIMIZED_VERT');
            add('_NET_WM_DESKTOP');
            add('_NET_NUMBER_OF_DESKTOPS');
            add('WM_CHANGE_STATE');
            add('SM_CLIENT_ID');
            add('WM_CLIENT_LEADER');
            add('WM_WINDOW_ROLE');
            add('_NET_CURRENT_DESKTOP');
            add('_NET_SUPPORTING_WM_CHECK');
            add('_NET_SUPPORTED');
            add('_NET_WM_WINDOW_TYPE');
            add('_NET_WM_WINDOW_TYPE_DESKTOP');
            add('_NET_WM_WINDOW_TYPE_DOCK');
            add('_NET_WM_WINDOW_TYPE_TOOLBAR');
            add('_NET_WM_WINDOW_TYPE_MENU');
            add('_NET_WM_WINDOW_TYPE_DIALOG');
            add('_NET_WM_WINDOW_TYPE_NORMAL');
            add('_NET_WM_STATE_MODAL');
            add('_NET_CLIENT_LIST');
            add('_NET_CLIENT_LIST_STACKING');
            add('_NET_WM_STATE_SKIP_TASKBAR');
            add('_NET_WM_STATE_SKIP_PAGER');
            add('_WIN_WORKSPACE');
            add('_WIN_LAYER');
            add('_WIN_PROTOCOLS');
            add('_WIN_SUPPORTING_WM_CHECK');
            add('_NET_WM_ICON_NAME');
            add('_NET_WM_ICON');
            add('_NET_WM_ICON_GEOMETRY');
            add('UTF8_STRING');
            add('WM_ICON_SIZE');
            add('_KWM_WIN_ICON');
            add('_NET_WM_MOVERESIZE');
            add('_NET_ACTIVE_WINDOW');
            add('_METACITY_RESTART_MESSAGE');
            add('_NET_WM_STRUT');
            add('_WIN_HINTS');
            add('_METACITY_RELOAD_THEME_MESSAGE');
            add('_METACITY_SET_KEYBINDINGS_MESSAGE');
            add('_NET_WM_STATE_HIDDEN');
            add('_NET_WM_WINDOW_TYPE_UTILITY');
            add('_NET_WM_WINDOW_TYPE_SPLASHSCREEN');
            add('_NET_WM_STATE_FULLSCREEN');
            add('_NET_WM_PING');
            add('_NET_WM_PID');
            add('WM_CLIENT_MACHINE');
            add('_NET_WORKAREA');
            add('_NET_SHOWING_DESKTOP');
            add('_NET_DESKTOP_LAYOUT');
            add('MANAGER');
            add('TARGETS');
            add('MULTIPLE');
            add('TIMESTAMP');
            add('VERSION');
            add('ATOM_PAIR');
            add('_NET_DESKTOP_NAMES');
            add('_NET_WM_ALLOWED_ACTIONS');
            add('_NET_WM_ACTION_MOVE');
            add('_NET_WM_ACTION_RESIZE');
            add('_NET_WM_ACTION_SHADE');
            add('_NET_WM_ACTION_STICK');
            add('_NET_WM_ACTION_MAXIMIZE_HORZ');
            add('_NET_WM_ACTION_MAXIMIZE_VERT');
            add('_NET_WM_ACTION_CHANGE_DESKTOP');
            add('_NET_WM_ACTION_CLOSE');
            add('_NET_WM_STATE_ABOVE');
            add('_NET_WM_STATE_BELOW');
            add('_KDE_NET_WM_SYSTEM_TRAY_WINDOW_FOR');
            add('_BY_PASS_WM');            
        end;

        for i:=0 to atomNames.count-1 do begin
            atomn[i]:=stralloc(length(atomNames[i])+1);
            strpcopy(atomn[i],atomNames[i]);
        end;

        XInternAtoms (Fdisplay, @atomn, atomNames.count, 0, @FAtoms);

        for i:=0 to atomNames.count-1 do begin
            strdispose(atomn[i]);
        end;
    finally
        atomNames.free;
    end;
end;

function TXPWindowManager.set_wm_icon_size_hint: integer;
const
    N_VALS=6;
var
    vals:array[0..N_VALS] of integer;
begin
  // min width, min height, max w, max h, width inc, height inc
  vals[0] := ICON_WIDTH;
  vals[1] := ICON_HEIGHT;
  vals[2] := ICON_WIDTH;
  vals[3] := ICON_HEIGHT;
  vals[4] := 0;
  vals[5] := 0;

  XChangeProperty (Fdisplay, Froot, Atoms[atom_wm_icon_size], XA_CARDINAL, 32, PropModeReplace, @vals, N_VALS);

  result:=Success;
end;

function TXPWindowManager.set_wm_check_hint:integer;
var
    data:array[0..1] of cardinal;
begin
  data[0] := FRoot;

  XChangeProperty (FDisplay, FRoot, atoms[atom_net_supporting_wm_check],
                   XA_WINDOW,
                   32, PropModeReplace, @data, 1);

  // Legacy GNOME hint (uses cardinal, dunno why)

  {* do this after setting up window fully, to avoid races
   * with clients listening to property notify on root.
   *}
  XChangeProperty (FDisplay, FRoot,
                   atoms[atom_win_supporting_wm_check],
                   XA_CARDINAL,
                   32, PropModeReplace, @data, 1);

  result:=Success;
end;

procedure TXPWindowManager.setupDisplay;
var
    buf: string;
    wm_sn_atom: Atom;
    wm_sn_owner: Window;
    attr:XWindowAttributes;
    attrs: XSetWindowAttributes;
    event: XEvent;
    manager_timestamp: TTime;

    i:integer;

    ev: XClientMessageEvent;

    ats: array[0..atomsSupported] of Atom;
begin
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,'TXPWindowManager.setupDisplay');
    {$endif}
//    XSynchronize (FDisplay, 1);

	FScreen:= XDefaultScreen(FDisplay);
	FRoot := XRootWindow(FDisplay, FScreen);

    buf:='WM_S'+inttostr(FScreen);
    wm_sn_atom := XInternAtom (FDisplay, PChar(buf), 0);
    wm_sn_owner := XGetSelectionOwner (FDisplay, wm_sn_atom);

    if (wm_sn_owner <> None) then begin
        XLibInterface.outputdebugstring(iWARNING,'There is already a window manager active!!');
        exit;
    end;


    // Generate a timestamp
    //attrs.event_mask := PropertyChangeMask;
    attrs.event_mask := ChildMask or PropertyChangeMask or ColormapChangeMask or ButtonMask;
    XChangeWindowAttributes (FDisplay, FRoot, CWEventMask, @attrs);

    XChangeProperty (FDisplay, FRoot, XA_WM_CLASS, XA_STRING, 8, PropModeAppend, nil, 0);

    XWindowEvent (FDisplay, FRoot, PropertyChangeMask, @event);

    attrs.event_mask := NoEventMask;

    XChangeWindowAttributes (FDisplay, FRoot, CWEventMask, @attrs);

    manager_timestamp := event.xproperty.time;

    XSetSelectionOwner (FDisplay, wm_sn_atom, FRoot, manager_timestamp);

    if (XGetSelectionOwner (FDisplay, wm_sn_atom) <> FRoot) then begin
      XLibInterface.OutputDebugString(iWARNING,'Could not acquire window manager selection');
      exit;
    end;

    // Send client message indicating that we are now the WM
    ev.xtype := ClientMessage;
    ev.xwindow := FRoot;
    ev.message_type := Atoms[atom_manager];
    ev.format := 32;
    ev.data.l[0] := longint(manager_timestamp);
    ev.data.l[1] := wm_sn_atom;

    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iINFO,'XSendEvent');
    {$endif}

    XSendEvent (FDisplay, FRoot, 0, StructureNotifyMask, @ev);

    XGetWindowAttributes (FDisplay, FRoot, @attr);
    XSelectInput (FDisplay, FRoot,
                  SubstructureRedirectMask or SubstructureNotifyMask or
                  ColormapChangeMask or PropertyChangeMask or
                  LeaveWindowMask or EnterWindowMask or
                  ButtonPressMask or ButtonReleaseMask or
                  KeyPressMask or KeyReleaseMask or
                  FocusChangeMask or StructureNotifyMask or attr.your_event_mask);


    set_wm_icon_size_hint;

    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iINFO,'XChangeProperty');
    {$endif}
    for i:=0 to atomsSupported do ats[i]:=FAtoms[i];
    XChangeProperty (Fdisplay, FRoot, Atoms[atom_net_supported], XA_ATOM, 32, PropModeReplace,@ats, atomsSupported);

    set_wm_check_hint;

end;

procedure TXPWindowManager.setupEventHandler;
begin
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,'TXPWindowManager.setupEventHandler');
    {$endif}
    oldevent:=application.SetX11EventFilter(eventhandler);
end;

procedure TXPWindowManager.setupSignals;
var
    act: TSigaction;
    empty_mask: sigset_t;
begin
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,'TXPWindowManager.setupSignals');
    {$endif}
    sigemptyset (empty_mask);
    act.__sigaction_handler := @handlesignal;
    act.sa_mask    := empty_mask;
    act.sa_flags   := 0;
    if (sigaction (SIGPIPE,  @act, nil) < 0) then xlibinterface.OutputDebugString(iERROR,'Failed to register SIGPIPE handler');
    if (sigaction (SIGXFSZ,  @act, nil) < 0) then xlibinterface.OutputDebugString(iERROR,'Failed to register SIGXFSZ handler');

end;

procedure TXPWindowManager.ungrabDisplay;
begin
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,'TXPWindowManager.ungrabdisplay, FServerGrabCount '+inttostr(FServerGrabcount));
    xlibinterface.outputDebugString(iENDPROCESS,'Ungrabdisplay');
    {$endif}
  if (FServerGrabCount = 0) then XLibInterface.outputdebugstring(iWARNING,'Ungrabbed non-grabbed server');

  dec(FServerGrabCount);

  if (FServerGrabCount= 0) then begin
      {* FIXME we want to purge all pending "queued" stuff
       * at this point, such as window hide/show
       *}
      XSync (FDisplay, 0);
      XUngrabServer (FDisplay);
  end;
  XSync (FDisplay, 0);
end;

procedure TXPWindowManager.setupErrorHandler;
begin
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,'TXPWindowManager.setupErrorHandler');
    {$endif}
    XSetErrorHandler(@handleXerror);
end;

procedure TXPWindowManager.setFrame(AFrameClass: TFormClass);
begin
//    FFrame:=AFrameClass;
end;

function TXPWindowManager.findClient(w: Window): TWMClient;
var
    i:longint;
    c: TWMClient;
begin
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iINFO,'Finding Client for window '+xlibinterface.formatwindow(w));
    {$endif}
    result:=nil;
    for i:=0 to clients.count-1 do begin
        c:=clients[i];
        if (c.xwindow=w) then begin
            result:=c;
            break;
        end;
    end;
    {$ifdef DEBUG}
    if assigned(result) then xlibinterface.outputDebugString(iINFO,'Client found for window '+xlibinterface.formatWindow(w))
    else begin
        xlibinterface.outputDebugString(iWARNING,'Client NOT found for window '+xlibinterface.formatWindow(w))
    end;
    {$endif}
end;

function TXPWindowManager.createNewClient(w: Window): TWMClient;
begin
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,'TXPWindowManager.createNewClient');
    {$endif}
    result:=TWMClient.create(w,self);
    if assigned(result) then begin
//        writeln('clients.insert');
        clients.insert(0,result);
//        writeln(clients.count);
        //Ensures the grabs are not overriden
//        writeln('setupKeyboardGrab');
        setupKeyboardGrab;
//        writeln('focus');
        result.focus;
//        writeln('end createnewclient');
    end;
end;

procedure TXPWindowManager.SetActiveClient(const Value: TWMClient);
var
    old: TWMClient;
    idx: integer;
begin
    //Here
    if FActiveClient<>Value then begin
        old:=FActiveClient;
        FActiveClient := Value;
        if assigned(old) then old.updateactivestate;
        if assigned(FActiveClient) then begin
            FActiveClient.updateactivestate;
            FActiveClient.updateactivewindow;
            if clients.count>1 then begin
//                writeln('SetActiveClient');
                if (clients.Remove(FActiveClient)<>-1) then clients.Insert(0,FActiveClient);
//                writeln(clients.count);                
            end;

        end
        else begin
            taskbar.activatetask(nil);
        end;
    end;
end;

procedure TXPWindowManager.setupKeyboardGrab;
begin
//    showmessage(inttostr(XGrabKey(FDisplay,XK_F6,AnyModifier,FRoot,0,grabmodeasync,grabmodesync)));
//    showmessage(inttostr(XGrabKeyboard(FDisplay,FRoot,1,grabmodeasync,grabmodesync,0)));
//    xgrabkeyboard(qtdisplay,xdefaultrootwindow(qtdisplay),1,grabmodeasync,grabmodeasync,0);
      XGrabKey(qtdisplay,XKeysymToKeycode(qtdisplay,XK_F4),Mod1Mask,xdefaultrootwindow(qtdisplay),1,grabmodeasync,grabmodeasync);
      XGrabKey(qtdisplay,XKeysymToKeycode(qtdisplay,XK_Tab),Mod1Mask,xdefaultrootwindow(qtdisplay),1,grabmodeasync,grabmodeasync);
//      XGrabKey(qtdisplay,AnyKey,Mod1Mask,xdefaultrootwindow(qtdisplay),1,grabmodeasync,grabmodeasync);      
end;

function TXPWindowManager.handleKeyRelease(var event: XEvent): integer;
begin
    showmessage('here');
end;

procedure TXPWindowManager.closeActiveWindow;
begin
    if assigned(FActiveClient) then FActiveClient.close;
end;

function TXPWindowManager.hasborder(xwindow: window): boolean;
const
    MwmHintsDecorations=1 shl 1;
    MwmDecorAll =  1;
    MwmDecorBorder= 1 shl 1;
    MwmDecorHandle = 1 shl 2;
    MwmDecorTitle  = 1 shl 3;
    MwmDecorMenu   = 1 shl 4;
    MwmDecorMinimize = 1 shl 5;
    MwmDecorMaximize = 1 shl 6;
    PropMotifWmHintsElements=3;
type
  TMwmHints=record
    flags:integer;
    functions:integer;
    decorations:integer;
  end;
var
  atype: Atom;
  format: integer;
  nitems:integer;
  bytes_after:integer;
  icons:PPixmap;
  apixmap: Pixmap;
  mask: Pixmap;
  err, res: integer;

  prop: TAtom;
  data: ^TMWMhints;

begin
    result:=true;
  prop := XInternAtom(Application.Display, '_MOTIF_WM_HINTS', 0);
  res := XGetWindowProperty (application.Display, xwindow, prop, 0, 20,  0, prop, @atype, @format, @nitems, @bytes_after, @data);

  if (res=success) and (nitems >= PropMotifWmHintsElements) then begin
    if ((data^.decorations and mwmDecorBorder)=mwmDecorBorder) then result:=true
    else result:=false;
    XFree (data);
  end;
end;

function TXPWindowManager.getFramedClients: TList;
var
    i: integer;
    cli: TWMClient;
    b: TBitmap;
begin
    clist.clear;
    for i:=0 to fclients.count-1 do begin
        cli:=fclients[i];
        b:=cli.geticon;
        if assigned(b) then clist.add(cli);
    end;
    result:=clist;
end;

function TXPWindowManager.findTransientFor(transient: Window): TWMClient;
var
    i:longint;
    c: TWMClient;
begin
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iINFO,'Finding transient for window '+xlibinterface.formatwindow(transient));
    {$endif}
    result:=nil;
    for i:=0 to clients.count-1 do begin
        c:=clients[i];
        if (c.xtransientfor=transient) then begin
            result:=c;
            break;
        end;
    end;
    {$ifdef DEBUG}
    if assigned(result) then xlibinterface.outputDebugString(iINFO,'Transient found for window '+xlibinterface.formatWindow(transient))
    else begin
        xlibinterface.outputDebugString(iWARNING,'Transient NOT found for window '+xlibinterface.formatWindow(transient))
    end;
    {$endif}
end;

procedure TXPWindowManager.sendalltoback(exclude: TList);
var
    i:longint;
    c: TWMClient;
begin
    for i:=clients.count-1 downto 0 do begin
        c:=clients[i];
        if exclude.IndexOf(c)=-1 then begin
            c.sendtoback;
        end;
    end;
end;

procedure TXPWindowManager.bypasswindow(xwindow: window);
var
    data: integer;
    res: integer;
begin
    data := 1;
//    Writeln('tobypass:'+inttostr(xwindow));
    res:=XChangeProperty(display, xwindow, Atoms[atom_BY_PASS_WM], Atoms[atom_BY_PASS_WM], 32, PropModeReplace, @data, 1);
end;

{ TWMClient }

function send_xmessage(w:Window;a:Atom;x:Atom):integer;
var
	ev:XEvent;
begin
	ev.xtype := ClientMessage;
	ev.xclient.xwindow := w;
	ev.xclient.message_type := a;
	ev.xclient.format := 32;
	ev.xclient.data.l[0] := x;
	ev.xclient.data.l[1] := CurrentTime;

	result:=XSendEvent(XPWindowManager.Display, w, 0, NoEventMask, @ev);
end;

procedure TWMClient.activate(restore:boolean=true);
var
    tf: TWMClient;
    exclude: TList;
begin
    tf:=FWindowManager.findTransientFor(xwindow);
    //If it's not modal, but there is a modal shown
    if (assigned(tf)) and (tf<>self) then begin
        {
        exclude:=TList.create;
        try
            exclude.add(self);
            exclude.add(tf);
            FWindowManager.sendalltoback(exclude);
            FWindowManager.ActiveClient:=self;
//          bringtofront;
            tf.activate(restore);
        finally
            exclude.free;
        end;
        }
        if FWindowManager.ActiveClient<>tf then begin
            bringtofront;
        end;
        tf.activate(restore);
    end
    else begin
        if (restore) then begin
            if FWindowState=wsMinimized then self.restore;
        end;
        //If it's modal
        if (xtransientfor<>None) then begin
            //Find the *parent*
            tf:=FWindowManager.findClient(xtransientfor);
            if assigned(tf) then begin
                {
                exclude:=TList.create;
                try
                    exclude.add(self);
                    exclude.add(tf);
                    FWindowManager.sendalltoback(exclude);
                    FWindowManager.ActiveClient:=tf;
                    //if assigned(tf) then tf.bringtofront;
                finally
                end;
                }
                if FWindowManager.ActiveClient<>self then begin
                    FWindowManager.ActiveClient:=tf;
                    tf.bringtofront;
                end;
            end;
        end;
        FWindowManager.ActiveClient:=self;
        updateactivestate;
        taskbar.activatetask(self);
        bringtofront;
        focus;
        //This causes Delphi to raise a "Call to an OS function failed"
        //It's due because the IDE tries to maximize the editor window
        //FIXIT!!!
        //Way to reproduce:
        //Save the default desktop with the editor window maximized (F5)
        //The editor window must be in MAXIMIZED STATE!!!
        sendsyntheticConfigureNotify;
    end;
end;

procedure TWMClient.beginresize;
begin
    inresize:=true;
end;

procedure TWMClient.bringtofront;
begin
    if not framed then begin
        XMapRaised(application.display,xwindow);
    end
    else begin
        if assigned(frame) then frame.BringToFront
        else XMapRaised(application.display,xwindow);
    end;
    taskbar.bringtofront;
    fwindowmanager.activeclient:=self;
end;

procedure TWMClient.close;
var
    i,n,found:integer;
	protocols : PAtom;
    init: PAtom;
begin
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,'TWMClient.close '+xlibinterface.formatwindow(xwindow)+format('[%s]',[(frame as TStyledFrame).gettitle]));
    {$endif}

	found := 0;
    if (XGetWMProtocols(FWindowManager.FDisplay, xwindow, @protocols, @n)<>0) then begin
            init:=protocols;
            try
                for i:=0 to n-1 do begin
                    if (protocols^ = FWindowManager.Atoms[atom_wm_delete_window]) then inc(found);
                    inc(protocols);
                end;
            finally
                protocols:=init;
		    	XFree(protocols);
            end;
    end;

    if found<>0 then begin
        send_xmessage(xwindow, FWindowManager.Atoms[atom_wm_protocols], FWindowManager.Atoms[atom_wm_delete_window]);
    end
    else begin
        XKillClient(FWindowManager.FDisplay, xwindow);
    end;
end;

procedure TWMClient.configure_request(event: XEvent);
var
  x, y, width, height:integer;
//  only_resize:boolean;
  allow_position_change:boolean;
//  in_grab_op:boolean;
  fbs: TRect;

    xwcm: cardinal;
    xwc: XWindowChanges;  
begin
    if not inresize then begin
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iBEGINPROCESS,'CONFIGURE_REQUEST');
    xlibinterface.outputDebugString(iINFO,'configure request for window '+xlibinterface.formatWindow(event.xconfigurerequest.xwindow)+format('[%s]',[(frame as TStyledFrame).gettitle]));
    {$endif}

    if assigned(frame) then begin
//*******************
  {* We ignore configure requests while the user is moving/resizing
   * the window, since these represent the app sucking and fighting
   * the user, most likely due to a bug in the app (e.g. pfaedit
   * seemed to do this)
   *
   * Still have to do the ConfigureNotify and all, but pretend the
   * app asked for the current size/position instead of the new one.
   *}
  (*
  in_grab_op = FALSE;
  if (window.display.grab_op != META_GRAB_OP_NONE &&
      window == window.display.grab_window)
    {
      switch (window.display.grab_op)
        {
        case META_GRAB_OP_MOVING:
        case META_GRAB_OP_RESIZING_SE:
        case META_GRAB_OP_RESIZING_S:
        case META_GRAB_OP_RESIZING_SW:
        case META_GRAB_OP_RESIZING_N:
        case META_GRAB_OP_RESIZING_NE:
        case META_GRAB_OP_RESIZING_NW:
        case META_GRAB_OP_RESIZING_W:
        case META_GRAB_OP_RESIZING_E:
          in_grab_op = TRUE;
          break;
        default:
          break;
        }
    }
    *)

  {* it's essential to use only the explicitly-set fields,
   * and otherwise use our current up-to-date position.
   *
   * Otherwise you get spurious position changes when the app changes
   * size, for example, if window.rect is not in sync with the
   * server-side position in effect when the configure request was
   * generated.
   *}

  (*
  meta_window_get_gravity_position (window, &x, &y);

  only_resize = TRUE;
  *)

  allow_position_change := true;

{
  if ((window.size_hints.flags & PPosition) ||
               /* USPosition is just stale if window is placed;
                * no --geometry involved here.
                */
               ((window.size_hints.flags & USPosition) &&
                !window.placed))
        allow_position_change = TRUE;
}

(*
  if (in_grab_op)
    allow_position_change = FALSE;
  *)
  fbs:=(frame as TStyledFrame).getframebordersizes;
  x:=frame.left;
  y:=frame.top;

  if (allow_position_change) then begin
      if ((event.xconfigurerequest.value_mask and CWX)=CWX) then x := event.xconfigurerequest.x;

      if ((event.xconfigurerequest.value_mask and CWY)=CWY) then y := event.xconfigurerequest.y;

      {
      if (event.xconfigurerequest.value_mask and (CWX or CWY)) then begin
          only_resize = FALSE;

          /* Once manually positioned, windows shouldn't be placed
           * by the window manager.
           */
          window.placed = TRUE;
      end;
      }
  end
  else begin
    {
      meta_topic (META_DEBUG_GEOMETRY,
		  "Not allowing position change for window %s PPosition 0x%x USPosition 0x%x type %d\n",
		  window.desc, window.size_hints.flags & PPosition,
		  window.size_hints.flags & USPosition,
		  window.type);
    }
  end;

  width := frame.Width-(fbs.left+fbs.right);
  height := frame.Height-(fbs.top+fbs.bottom);

//  if (!in_grab_op) then begin
      if ((event.xconfigurerequest.value_mask and CWWidth)=CWWidth) then width := event.xconfigurerequest.width;

      if ((event.xconfigurerequest.value_mask and CWHeight)=CWHeight) then height := event.xconfigurerequest.height;
//  end;

  // ICCCM 4.1.5

  {* Note that x, y is the corner of the window border,
   * and width, height is the size of the window inside
   * its border, but that we always deny border requests
   * and give windows a border of 0. But we save the
   * requested border here.
   *}
  //window.border_width = event.xconfigurerequest.border_width;

  {* We're ignoring the value_mask here, since sizes
   * not in the mask will be the current window geometry.
   *}

  {
  window.size_hints.x = x;
  window.size_hints.y = y;
  window.size_hints.width = width;
  window.size_hints.height = height;
  }

  {* FIXME passing the gravity on only_resize thing is kind of crack-rock.
   * Basically I now have several ways of handling gravity, and things
   * don't make too much sense. I think I am doing the math in a couple
   * places and could do it in only one function, and remove some of the
   * move_resize_internal arguments.
   *}

   {
  meta_window_move_resize_internal (window, META_IS_CONFIGURE_REQUEST,
                                    only_resize ?
                                    window.size_hints.win_gravity : NorthWestGravity,
                                    window.size_hints.x,
                                    window.size_hints.y,
                                    window.size_hints.width,
                                    window.size_hints.height);
  }
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iINFO,format('CONFIGURE_REQUEST: setting frame to (%d,%d)-(%d,%d) (some values are not set)',[x,y,width,height]));
    {$endif}

  if ((event.xconfigurerequest.value_mask and CWX)=CWX) then frame.Left:=x;
  if ((event.xconfigurerequest.value_mask and CWY)=CWY) then frame.top:=y;
  if ((event.xconfigurerequest.value_mask and CWWidth)=CWWidth) then frame.width:=width+(fbs.Left+fbs.right);
  if ((event.xconfigurerequest.value_mask and CWHeight)=CWHeight) then frame.height:=height+(fbs.top+fbs.bottom);

  if FWindowState=wsNormal then begin
    wRect:=frame.boundsrect;
  end;

    if ((event.xconfigurerequest.value_mask and CWWidth)=CWWidth) or ((event.xconfigurerequest.value_mask and CWHeight)=CWHeight) then begin
        {$ifdef DEBUG}
        xlibinterface.outputDebugString(iINFO,format('CONFIGURE_REQUEST: resizing window %s to (%d,%d)',[xlibinterface.formatwindow(xwindow),width,height]));
        {$endif}
        XResizeWindow(FWindowManager.FDisplay,xwindow,width,height);
    end;
  end;

  if not framed then begin
        xwcm := event.xconfigurerequest.value_mask and (CWX or CWY or CWWidth or CWHeight or CWBorderWidth);

        xwc.x := event.xconfigurerequest.x;
        xwc.y := event.xconfigurerequest.y;
        xwc.width := event.xconfigurerequest.width;
        xwc.height := event.xconfigurerequest.height;
        xwc.border_width := event.xconfigurerequest.border_width;

        {$ifdef DEBUG}
        xlibinterface.outputDebugString(iINFO,format('Configuring withdrawn window %s to %d,%d %dx%d border %d (some values may not be in mask)',[xlibinterface.formatwindow(xwindow),xwc.x, xwc.y, xwc.width, xwc.height, xwc.border_width]));
        xlibinterface.outputDebugString(iINFO,format('%s to %d',[xlibinterface.formatwindow(event.xconfigurerequest.above),event.xconfigurerequest.detail]));
        {$endif}

        XConfigureWindow (FWindowManager.FDisplay, event.xconfigurerequest.xwindow, xwcm, @xwc);
  end;

  {* Handle stacking. We only handle raises/lowers, mostly because
   * stack.c really can't deal with anything else.  I guess we'll fix
   * that if a client turns up that really requires it. Only a very
   * few clients even require the raise/lower (and in fact all client
   * attempts to deal with stacking order are essentially broken,
   * since they have no idea what other clients are involved or how
   * the stack looks).
   *
   * I'm pretty sure no interesting client uses TopIf, BottomIf, or
   * Opposite anyway, so the only possible missing thing is
   * Above/Below with a sibling set. For now we just pretend there's
   * never a sibling set and always do the full raise/lower instead of
   * the raise-just-above/below-sibling.
   *}

  if ((event.xconfigurerequest.value_mask and CWStackMode)=CWStackMode) then begin
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iINFO, 'Stack mode');
    {$endif}
      case (event.xconfigurerequest.detail) of
        Above: begin
            bringtofront;
//            activate;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iINFO, 'bringtofront');
    {$endif}
        end;
        Below: begin
            sendtoback;
            //frame.sendtoback;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iINFO, 'sendtoback');
    {$endif}
        end;
      end;
  end;

    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iENDPROCESS,'CONFIGURE_REQUEST');
    {$endif}
  end;
end;

constructor TWMClient.Create(AWindow: Window;
  AWindowManager: TXPWindowManager);
begin
    inherited Create;
    framed:=true;
    inresize:=false;
    minimizedstate:=wsNormal;
    FWindowState:=wsNormal;
    xwindow:=AWindow;
    frame:=nil;
    wRect:=Rect(0,0,0,0);
    FUnmapcounter:=0;
    FWindowManager:=AWindowManager;
    //Grab the three mouse buttons

      XGrabButton (FWindowManager.FDisplay, 1, 0, awindow, 0,
                     ButtonPressMask or ButtonReleaseMask or
                     PointerMotionMask or PointerMotionHintMask,
                     GrabModeSync, GrabModeAsync, 0, None);
      XGrabButton (FWindowManager.FDisplay, 2, 0, awindow, 0,
                     ButtonPressMask or ButtonReleaseMask or
                     PointerMotionMask or PointerMotionHintMask,
                     GrabModeSync, GrabModeAsync, 0, None);
      XGrabButton (FWindowManager.FDisplay, 3, 0, awindow, 0,
                     ButtonPressMask or ButtonReleaseMask or
                     PointerMotionMask or PointerMotionHintMask,
                     GrabModeSync, GrabModeAsync, 0, None);
    createFrame;
end;

procedure TWMClient.createFrame;
var
	attr:XWindowAttributes;
    attrs:XSetWindowAttributes;
	dummy:longint;
	hints: PXWMHints;
    sizehints: PXSizeHints;
	name:XTextProperty;
    data: PChar;
    str: TMemoryStream;
//    nw,nh: longint;
    state: variant;
    fbs: TRect;
begin
    hasBorder;
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,'TWMClient.createframe');
    {$endif}
    FWindowManager.grabDisplay;
	hints := XGetWMHints(FWindowManager.FDisplay, xwindow);
    sizehints:=XAllocSizeHints();
    XGetWMNormalHints(FWindowManager.FDisplay, xwindow, sizehints, @dummy);
	XGetWindowAttributes(FWindowManager.FDisplay, xwindow, @attr);
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iINFO,format('*****attr (%d,%d) %dx%d',[attr.x,attr.y,attr.width,attr.height]));
    {$endif}

    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iINFO,format('attr.override_redirect:%d',[attr.override_redirect]));
    {$endif}

    if (attr.override_redirect<>0) then begin
        {$ifdef DEBUG}
      xlibinterface.outputdebugstring(iINFO,format('Deciding not to manage override_redirect window %s',[xlibinterface.formatwindow(xwindow)]));
      {$endif}
      FWindowManager.ungrabDisplay;
      exit;
    end;

    XSelectInput (FWindowManager.FDisplay, xwindow, PropertyChangeMask or EnterWindowMask or LeaveWindowMask or FocusChangeMask);
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iINFO,format('attr.border_width:%d',[attr.border_width]));
    {$endif}
    // Get rid of any borders
    if (attr.border_width <> 0) then XSetWindowBorderWidth (FWindowManager.FDisplay, xwindow, 0);
    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iINFO,format('attr.win_gravity:%d',[attr.win_gravity]));
    {$endif}
    // Get rid of weird gravities
    if (attrs.win_gravity <> NorthWestGravity) then begin
      attrs.win_gravity := NorthWestGravity;

      XChangeWindowAttributes (FWindowManager.FDisplay, xwindow, CWWinGravity, @attrs);
    end;

    updatetransientfor;
    getMapState;



    if isKDETray then begin
    	if (attr.map_state = IsViewable) then inc(FUnmapcounter,1);
        //XPTaskBar.addwindowtotray(xwindow);
    end
    else begin
        frame:=TStyledFrame.create(application);
        (frame as TStyledFrame).setclient(self);

        if (assigned(hints)) and ((hints^.flags and IconPixmapHint)=IconPixmapHint) then begin
            XpmCreateBufferFromPixmap(FWindowManager.Display,data, hints^.icon_pixmap,hints^.icon_mask,nil);
            str:=TMemoryStream.create;
            try
                str.Write(data^,strlen(data));
                str.position:=0;
                (frame as TStyledFrame).imgIcon.Picture.bitmap.LoadFromStream(str);
                //(frame as TWindowsClassic).imgIcon.Picture.bitmap.savetofile('/home/ttm/icon.bmp');
            finally
                str.free;
            end;
        end
        else begin
           (frame as TStyledFrame).imgIcon.Picture.bitmap.LoadFromFile(getSystemInfo(XP_MISC_DIR)+'/noglyph.png');
        end;
        (frame as TStyledFrame).setupIcons;
        {$ifdef DEBUG}
        xlibinterface.outputDebugString(iINFO,format('Frame(%d,%d)-(%d,%d)',[attr.x,attr.y, attr.width,attr.height]));
        {$endif}


        fbs:=(frame as TStyledFrame).getFrameBorderSizes;

        {
        writeln('x:'+inttostr(attr.x));
        writeln('y:'+inttostr(attr.y));
        writeln('width:'+inttostr(attr.width));
        writeln('height:'+inttostr(attr.height));
        writeln('----------');
        }

        frame.left:=attr.x-fbs.left;
        if frame.left<0 then frame.left:=0;

        frame.top:=attr.y-fbs.top;
        if frame.top<0 then frame.top:=0;

        frame.width:=attr.width+(fbs.left+fbs.right);
        frame.height:=attr.height+(fbs.top+fbs.bottom);


        if (assigned(sizehints)) and (framed) then begin
            if (sizehints^.flags and PMinSize)=PMinSize then begin
                if sizehints^.min_width>frame.constraints.MinWidth then frame.Constraints.MinWidth:=sizehints^.min_width+(fbs.left+fbs.right);
                if sizehints^.min_height>frame.Constraints.MinHeight then frame.Constraints.MinHeight:=sizehints^.min_height+(fbs.top+fbs.bottom);
            end;

            if (sizehints^.flags and PMaxSize)=PMaxSize then begin
                if sizehints^.max_width>=sizehints^.min_width then frame.Constraints.maxWidth:=sizehints^.max_width+(fbs.left+fbs.right);
                if sizehints^.max_height>=sizehints^.min_height then frame.Constraints.maxHeight:=sizehints^.max_height+(fbs.top+fbs.bottom);
            end;
        end;

        if FWindowState=wsNormal then begin
            wRect:=frame.boundsrect;
        end;

        XGetWMName(FWindowManager.FDisplay, xwindow, @name);

        (frame as TStyledFrame).setTitle(listtostr(PChar(name.value)));

        if framed then begin
            frame.show;
        end;
        bringtofront;

        taskbar.addtask(self);
        taskbar.activatetask(self);

    	if (attr.map_state = IsViewable) then inc(FUnmapcounter,1)
        else begin
            {
    //		c.initPosition;
    		if (assigned(hints)) and  ((hints.flags and StateHint)<>0) then begin
                writeln(hints.initial_state);
            end;
            }
    	end;

        if framed then reparent;
        //A bit of hack, shit!
        if (attr.width>=qforms.Screen.Width) then maximize;

        {$ifdef DEBUG}
        xlibinterface.outputDebugString(iINFO,format('Resizing window to (%d,%d)',[attr.width,attr.height]));
        xlibinterface.outputDebugString(iINFO,format('Frame size (%d,%d)',[frame.width,frame.height]));
        {$endif}
        //XResizeWindow(FWindowManager.FDisplay,xwindow,sizehints.width,sizehints.height);
        map;
    end;


	if assigned(hints) then XFree(hints);
	if assigned(sizehints) then XFree(sizehints);
	XSync(FWindowManager.FDisplay, 0);
    FWindowManager.ungrabDisplay;

end;

destructor TWMClient.Destroy;
begin
  {$ifdef DEBUG}
  xlibinterface.outputDebugString(iMETHOD,'TWMClient.destroy');
  {$endif}
  if assigned(frame) then begin
    WindowManager.grabdisplay;

	XRemoveFromSaveSet(WindowManager.Display, xwindow);
	XReparentWindow(WindowManager.Display, xwindow, WindowManager.Root, frame.left, frame.top);
	XSetWindowBorderWidth(WindowManager.Display, xwindow, 1);

    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,'RemoveTask');
    {$endif}
    taskbar.removetask(self);

    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,'Frame.Free and nil');
    {$endif}
    frame.free;
    frame:=nil;

    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,'XSync');
    {$endif}
	XSync(WindowManager.Display, 0);

    {$ifdef DEBUG}
    xlibinterface.outputDebugString(iMETHOD,'UngrabDisplay');
    {$endif}
    WindowManager.ungrabdisplay;
  end
  else begin
    // XPTaskbar.removewindowfromtray(xwindow);
  end;


  inherited;
end;

procedure TWMClient.endresize;
begin
    inresize:=false;
end;

procedure TWMClient.focus;
begin
    {$ifdef DEBUG}
    if assigned(frame) then begin
        xlibinterface.outputDebugString(iMETHOD,'TWMClient.focus '+xlibinterface.formatwindow(xwindow)+format('[%s]',[(frame as TStyledFrame).gettitle]));
    end;
    {$endif}
    XSetInputFocus (WindowManager.Display,xwindow, RevertToPointerRoot, CurrentTime);
end;

function TWMClient.getBitmap: TBitmap;
begin
    result:=(frame as TStyledFrame).smallIcon;
end;

function TWMClient.getIcon: TBitmap;
begin
    if assigned(frame) then begin
        result:=(frame as TStyledFrame).normalIcon;
    end
    else begin
        result:=nil;
    end;
end;

procedure TWMClient.getIcons;
var
  atype: Atom;
  format: integer;
  nitems:integer;
  bytes_after:integer;
  icons:PPixmap;
  apixmap: Pixmap;
  mask: Pixmap;
  err, result: integer;
begin
{
static void
get_kwm_win_icon (MetaDisplay *display,
                  Window       xwindow,
                  Pixmap      *pixmap,
                  Pixmap      *mask)
}

  apixmap := None;
  mask := None;

  result := XGetWindowProperty (FWindowManager.FDisplay, xwindow, FWindowManager.Atoms[atom_kwm_win_icon],
			       0, MaxInt,  0, FWindowManager.Atoms[atom_kwm_win_icon], @atype, @format, @nitems, @bytes_after, @icons);


  if (atype <> FWindowManager.Atoms[atom_kwm_win_icon]) then begin
      XFree (icons);
      exit;
  end;

  apixmap := icons^;
  inc(PChar(icons),4);
  mask := icons^;
  dec(PChar(icons),4);


  XFree (icons);

end;

procedure TWMClient.getMapState;
var
    data: PAtom;
  aformat: integer;
  nitems:integer;
  atype: integer;
  bytes_after:integer;
  res: integer;    
begin

{
  prop := XInternAtom(Application.Display, '_MOTIF_WM_HINTS', 0);
  res := XGetWindowProperty (application.Display, xwindow, prop, 0, 20,  0, prop, @atype, @format, @nitems, @bytes_after, @data);

  if (res=success) and (nitems >= PropMotifWmHintsElements) then begin
    if ((data^.decorations and mwmDecorBorder)=mwmDecorBorder) then result:=true
    else result:=false;
    XFree (data);
  end;
  }
{
  data[0] := state;
  data[1] := 0;
}

  res:=XGetWindowProperty(FWindowManager.Display, xwindow, FWindowManager.Atoms[atom_wm_state], 0,20,0,XA_ATOM, @atype, @aformat, @nitems, @bytes_after,@data);
  if (res=success) then begin
//        writeln('wm_state!!!! '+inttostr(nitems));
        XFree(data);  
  end;
end;

procedure TWMClient.getSizeHints;
begin

end;

function TWMClient.getSystemMenu: TPopupMenu;
begin
    result:=nil;
//    result:=(frame as TWindowsClassic).PopupMenu1;
end;

function TWMClient.getTitle: widestring;
begin
    if assigned(frame) then begin
        result:=(frame as TStyledFrame).gettitle;
    end;
end;

function TWMClient.getWindow: Window;
begin
    result:=xwindow;
end;

procedure TWMClient.gravitate;
begin

end;

function TWMClient.hasBorder: boolean;
const
    MwmHintsDecorations=1 shl 1;
    MwmDecorAll =  1;
    MwmDecorBorder= 1 shl 1;
    MwmDecorHandle = 1 shl 2;
    MwmDecorTitle  = 1 shl 3;
    MwmDecorMenu   = 1 shl 4;
    MwmDecorMinimize = 1 shl 5;
    MwmDecorMaximize = 1 shl 6;
    PropMotifWmHintsElements=3;
type
  TMwmHints=record
    flags:integer;
    functions:integer;
    decorations:integer;
  end;
var
  atype: Atom;
  format: integer;
  nitems:integer;
  bytes_after:integer;
  icons:PPixmap;
  apixmap: Pixmap;
  mask: Pixmap;
  err, res: integer;

  prop: TAtom;
  data: ^TMWMhints;

begin
    result:=true;
  prop := XInternAtom(Application.Display, '_MOTIF_WM_HINTS', 0);
  res := XGetWindowProperty (application.Display, xwindow, prop, 0, 20,  0, prop, @atype, @format, @nitems, @bytes_after, @data);

  {
    writeln('-----------------');
  xlibinterface.outputDebugString(iMETHOD,sysutils.format('MWMHints %s',[xlibinterface.formatwindow(xwindow)]));
    writeln('-----------------');
    }

  if (res=success) and (nitems >= PropMotifWmHintsElements) then begin
    {
    writeln('flags:'+inttostr(data^.flags));
    writeln('functions:'+inttostr(data^.functions));
    writeln('decorations:'+inttostr(data^.decorations));
    writeln('-----------------');
    }
    if ((data^.decorations and mwmDecorAll)=mwmDecorAll) then result:=true
    else begin
        if ((data^.decorations and mwmDecorBorder)=mwmDecorBorder) then result:=true
        else result:=false;
    end;
    XFree (data);
  end;
  framed:=result;
end;

function TWMClient.isactive: boolean;
begin
    if assigned(FWindowManager.ActiveClient) then begin
        result:=(FWindowManager.ActiveClient=self);
    end
    else result:=false;
end;

function TWMClient.isframed: boolean;
begin
    result:=framed;
end;

function TWMClient.isKDETray: boolean;
var
  atype: Atom;
  format: integer;
  nitems:integer;
  bytes_after:integer;
  icons:PPixmap;
  apixmap: Pixmap;
  mask: Pixmap;
  err, res: integer;

  prop: TAtom;
  data: TAtom;

begin
(*
01417 bool NetHandler::IsSystrayWindow(Window w) {
01418     CARD32 *data;
01419
01420     items_read = 0;
01421     XGrabServer(display);
01422     if (validatedrawable(w)) {
01423         if (XGetWindowProperty(display, w, kde_net_wm_system_tray_window_for,
01424                                0L, 1L, false, XA_WINDOW, &real_type,
01425                                &real_format, &items_read, &items_left,
01426                                &data) != Success) {
01427             items_read = 0;
01428         }
01429     }
01430     XUngrabServer(display);
01431
01432     return ((items_read)? true: false);
01433 }
*)

  prop := XInternAtom(Application.Display, '_KDE_NET_WM_SYSTEM_TRAY_WINDOW_FOR', 1);
//         XChangeProperty(Application.Display, QWidget_winId(self.Handle), prop, XA_WINDOW, 32, PropModeReplace, PByte(@data), 1);

  res := XGetWindowProperty (FWindowManager.FDisplay, xwindow, prop, 0, 1,  0, XA_WINDOW, @atype, @format, @nitems, @bytes_after, @icons);

  if (nitems = 0) then begin
      result:=false;
  end
  else result:=true;

  XFree (icons);

end;

procedure TWMClient.map;
begin
	XMapWindow(FWindowManager.FDisplay, xwindow);
end;

procedure TWMClient.maximize;
var
    r: TRect;
    fbs: TRect;
begin
    if FWindowState<>wsMaximized then begin
        if FWindowState=wsNormal then begin
            wrect:=frame.boundsrect;
        end;
        {$ifdef DEBUG}
        xlibinterface.outputDebugString(iMETHOD,'TWMClient.maximize'+xlibinterface.formatwindow(xwindow)+format('[%s]',[(frame as TStyledFrame).gettitle]));
        {$endif}
        r:=FWindowManager.getDesktopClientRect;
        fbs:=(frame as TStyledFrame).getFrameBorderSizes;
        (*
        r.left:=r.left-fbs.left;
        r.top:=r.top-fbs.top;
        r.right:=r.right+fbs.right;
        r.bottom:=r.bottom+fbs.bottom;
        *)
        frame.BoundsRect:=r;
        XResizeWindow(FWindowManager.FDisplay, xwindow, frame.width-(fbs.left+fbs.right), frame.height-(fbs.top+fbs.bottom));
        FWindowState:=wsMaximized;
        (frame as TStyledFrame).updateWindowState;
    end;
end;

procedure TWMClient.minimize;
var
    p: TWMClient;
    i: integer;
    found: boolean;
begin
    if FWindowState<>wsMinimized then begin
            minimizedstate:=FWindowState;
            {$ifdef DEBUG}
            xlibinterface.outputDebugString(iMETHOD,'TWMClient.minimize'+xlibinterface.formatwindow(xwindow)+format('[%s]',[(frame as TStyledFrame).gettitle]));
            {$endif}

            if (framed) then begin
                frame.visible:=false;
            end
            else begin
                setMapState(IconicState);
                UnmapCounter:=UnmapCounter+1;
                XUnmapWindow(FWindowManager.Display,xwindow);
            end;
            FWindowState:=wsMinimized;

            found:=false;
            if FWindowManager.clients.count>1 then begin
                for i:=1 to FWindowManager.clients.count-1 do begin
                    p:=FWindowManager.clients[i];
                    if assigned(p) then begin
                        if (p.WindowState<>wsMinimized) then begin
                            p.activate;
                            writeln('found');
                            found:=true;
                            break;
                        end;
                    end;
                end;
            end;

            if not found then FWindowManager.activeclient:=nil;
    end;
end;

procedure TWMClient.reparent;
var
    p_attr:XSetWindowAttributes;
    parent: window;
    fbs: TRect;
begin
    if not assigned(frame) then exit;

	XSelectInput(FWindowManager.Fdisplay, xwindow, ColormapChangeMask or EnterWindowMask or PropertyChangeMask);

	p_attr.override_redirect := 1;
	p_attr.event_mask := ChildMask or ButtonPressMask or ExposureMask or EnterWindowMask;



    if isframed then begin
        parent:=QWidget_winID((frame as TStyledFrame).getHostControl.Handle);

        fbs:=(frame as TStyledFrame).getFrameBorderSizes;

    	XAddToSaveSet(FWindowManager.Fdisplay, xwindow);
    	XSetWindowBorderWidth(FWindowManager.Fdisplay, xwindow, 0);
        XChangeWindowAttributes(FWindowManager.Fdisplay,parent,CWOverrideRedirect or CWEventMask, @p_attr);

    	// Hmm, why resize this?
        XResizeWindow(FWindowManager.FDisplay, xwindow, frame.width-(fbs.left+fbs.right), frame.height-(fbs.top+fbs.bottom));
    	XReparentWindow(FWindowManager.Fdisplay, xwindow, parent, 0, 0);
    end;

    setMapState(NormalState);
end;

procedure TWMClient.resize;
begin

end;

procedure TWMClient.restore;
var
    fbs: TRect;
begin
    if assigned(frame) then begin
        if FWindowState<>wsNormal then begin
            {$ifdef DEBUG}
            xlibinterface.outputDebugString(iMETHOD,'TWMClient.restore'+xlibinterface.formatwindow(xwindow)+format('[%s]',[(frame as TStyledFrame).gettitle]));
            {$endif}
            if FWindowState=wsMinimized then begin
                if (framed) then frame.visible:=true
                else begin
                    XMapWindow(FWindowManager.FDisplay,xwindow);
                end;
                FWindowState:=minimizedstate;
            end
            else begin
                fbs:=(frame as TStyledFrame).getFrameBorderSizes;
                frame.BoundsRect:=wRect;
                XResizeWindow(FWindowManager.FDisplay, xwindow, frame.width-(fbs.left+fbs.right), frame.height-(fbs.top+fbs.bottom));
                FWindowState:=wsNormal;
            end;
            (frame as TStyledFrame).updateWindowState;
        end;
    end;
end;

procedure TWMClient.sendsyntheticConfigureNotify;
var
    c: XConfigureEvent;
    fbs: TRect;
begin
    if not assigned(frame) then exit;
    if not framed then exit;
    fbs:=(frame as TStyledFrame).getframebordersizes;

    c.xtype := ConfigureNotify;
    c.send_event := 1;
    c.event := xwindow;
    c.xwindow := xwindow;
    c.x := frame.left+fbs.left;
    c.y := frame.top+fbs.top;
    c.width := frame.width-(fbs.Left+fbs.right);
    c.height := frame.height-(fbs.top+fbs.bottom);
    c.border_width := 0;
    c.above := None;
    c.override_redirect := 0;


    {
    writeln('xwindow: '+inttostr(xwindow));
    writeln('caption: '+(frame as TWindowsClassic).gettitle);
    writeln('c.x: '+inttostr(c.x));
    writeln('c.y: '+inttostr(c.y));
    writeln('c.width: '+inttostr(c.width));
    writeln('c.height: '+inttostr(c.height));
    writeln('------------------');
    }

    XSendEvent( FWindowManager.FDisplay, c.xwindow, 0, StructureNotifyMask, @c );
end;

procedure TWMClient.sendtoback;
var
    c: TWMClient;
begin
    if not framed then begin
        //XMapRaised(application.display,xwindow);
    end
    else begin
        if assigned(frame) then begin
            c:=FWindowManager.framedclients[FWindowManager.framedclients.count-1];
            if assigned(c) then begin
//                writeln((c.frame as TWindowsClassic).getTitle);
//                QWidget_stackUnder(frame.Handle, c.frame.handle);
                QWidget_lower(frame.Handle);
            end;
        end
        else begin
            //XMapRaised(application.display,xwindow);
        end;
    end;
    taskbar.bringtofront;
end;

procedure TWMClient.setMapState(state: integer);
var
    data: array[0..1] of integer;
begin
  data[0] := state;
  data[1] := 0;

  XChangeProperty(FWindowManager.Display, xwindow, FWindowManager.Atoms[atom_wm_state], FWindowManager.Atoms[atom_wm_state], 32, PropModeReplace, @data, 2);
end;

procedure TWMClient.SetWindowManager(const Value: TXPWindowManager);
begin
  FWindowManager := Value;
end;

procedure TWMClient.SetWindowState(const Value: TWindowState);
begin
    if (FWindowState<>Value) then begin
      FWindowState := Value;
      case FWindowState of
        wsNormal: begin
            restore;
            setmapstate(NormalState);
        end;
        wsMinimized: begin
            minimize;
            setmapstate(IconicState);
        end;
        wsMaximized: begin
            maximize;
            setmapstate(NormalState);
        end;
      end;
    end;
end;

procedure TWMClient.updateactivestate;
begin
    if not assigned(frame) then exit;
    if assigned(frame) then begin
        (frame as TStyledFrame).updateactivestate;
    end;
end;

procedure TWMClient.updateactivewindow;
var
    data: array [0..1] of integer;
begin
    data[0] := xwindow;
    data[1] := None;

    XChangeProperty (FWindowManager.FDisplay, FWindowManager.Root, FWindowManager.Atoms[atom_net_active_window], XA_WINDOW, 32, PropModeReplace, @data, 2);
end;

procedure TWMClient.updatetransientfor;
var
    w: window;
begin
  w := None;
  XGetTransientForHint (FWindowManager.Display, xwindow, @w);
  xtransientfor := w;

  {
  window->transient_parent_is_root_window =
    window->xtransient_for == window->screen->xroot;
    }

    {$ifdef DEBUG}
    if (xtransientfor<>None) then begin
        xlibinterface.outputDebugString(iINFO,format('Window [%s] is transient for [%s]',[xlibinterface.formatwindow(xwindow),xlibinterface.formatwindow(w)]));
    end
    else begin
        xlibinterface.outputDebugString(iINFO,format('Window is not transient [%s]',[xlibinterface.formatwindow(xwindow)]));
    end;
    {$endif}
end;

{ TXLibInterface }

procedure TXLibInterface.outputDebugString(const kind:integer;const str: string);
var
    rs:string;
begin
//    {$ifdef DEBUG}
    case kind of
        iMETHOD:  rs:='METHOD  :'+str;
        iINFO:    rs:='INFO    :'+str;
        iWARNING: rs:='WARNING :'+str;
        iERROR:   rs:='ERROR   :'+str;
        iEVENT:   rs:='EVENT   :'+str;
        iBEGINPROCESS : begin
            inc(indent,10);
            rs:=stringofchar('>',20)+str;
        end;
        iENDPROCESS   : begin
            rs:=stringofchar('<',20)+str;
        end;
    end;
    writeln(stringofchar(' ',indent)+rs);
    if kind=iENDPROCESS then dec(indent,10);
//    {$endif}
end;

function TXLibInterface.GetWindowProperty(xdisplay:PDisplay;xwindow:Window;xatom:Atom;req_type:Atom):variant;
var
    treturn: Atom;
    freturn: longint;
    nitems,bafter: cardinal;
    preturn: PChar;
begin
    XGetWindowProperty(xdisplay,xwindow,xatom,0,MaxInt,0,req_type,@treturn,@freturn,@nitems,@bafter,@preturn);
    result:=cardinal(preturn^);
    XFree(preturn);
end;

//Returns the value of a window property, if exists
function TXLibInterface.getSimpleProperty(w:window;a:atom):longint;
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

function TXLibInterface.formatWindow(w: Window): string;
begin
    result:=format('($%s)',[inttohex(w,8)]);
end;

constructor TXLibInterface.Create;
begin
    inherited;
    indent:=0;
end;

initialization
    XLibInterface:=TXLibInterface.create;
    XPWindowManager:=TXPWindowManager.create;


finalization
    XPWindowManager.free;
    XLibInterface.free;

end.
