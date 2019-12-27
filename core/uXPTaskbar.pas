unit uXPTaskbar;

interface

uses
    Classes, QExtCtrls, uCommon,
    QControls, uXPStartButton, uXPSysTray,
    uXPTaskband, uWMConsts, QDialogs,
    SysUtils, QForms;

type
    TXPSizeGrip=class;

    TXPTaskbar=class(TPanel)
    public
        leftGrip:TXPSizeGrip;
        startButton: TXPStartButton;
        systray: TXPSysTray;
        taskband: TXPTaskBand;
        procedure activatetask(const task:IWMClient);
        procedure updatetask(const client:IWMClient);
        procedure addtask(const client:IWMClient);
        procedure removetask(const task:IWMClient);
        procedure onShowStartMenu(sender: TObject);
        procedure onCloseMenu(sender: TObject);
        procedure setup;
        constructor Create(AOwner:TComponent);override;
        destructor Destroy;override;
        { TODO : Add an orientation property }
    end;

    TXPSizeGrip=class(TPanel)
    public
        procedure setup;
        constructor Create(AOwner:TComponent);override;
        destructor Destroy;override;    
    end;

implementation

uses uXPStartMenu;

{ TXPTaskbar }

procedure TXPTaskbar.activatetask(const task: IWMClient);
begin
    taskband.activatetask(task);
end;

procedure TXPTaskbar.addtask(const client: IWMClient);
begin
    taskband.addtask(client);
end;

constructor TXPTaskbar.Create(AOwner: TComponent);
begin
  inherited;
  setup;
end;

destructor TXPTaskbar.Destroy;
begin
  leftGrip.free;
  startButton.free;
  sysTray.free;
  taskband.free;
  inherited;
end;

procedure TXPTaskbar.onCloseMenu(sender: TObject);
var
    st: TStartMenu;
begin
//Not needed now, the menu is always present
(*
    startButton.release;
    st:=getStartMenu;
    st.visible:=false;
    application.processmessages;
    freeStartMenu;
    self.Parent.BringToFront;
    self.Parent.setfocus;
    self.Parent.SetFocus;
*)
end;

procedure TXPTaskbar.onShowStartMenu(sender: TObject);
var
    st: TStartMenu;
begin
    //Get/Create the start menu and show it
    st:=getStartMenu;
    st.left:=0;
    st.top:=self.parent.top-st.height;
    st.OnHide:=onCloseMenu;
    st.visible:=true;
    st.BringToFront;
//    SetMouseGrabControl(st);
//    SetMouseGrabControl(nil);
end;

procedure TXPTaskbar.removetask(const task: IWMClient);
begin
    taskband.removetask(task);
end;

procedure TXPTaskbar.setup;
var
    dir: string;
begin
    leftGrip:=TXPSizeGrip.create(nil);
    leftGrip.align:=alLeft;
    leftGrip.parent:=self;

    BevelOuter:=bvNone;
    dir:=getSystemInfo(XP_TASKBAR_DIR);
    { TODO : Configure according the orientation property }
    bitmap.LoadFromFile(dir+'/taskbar_background_bottom.png');
    startButton:=TXPStartButton.create(nil);
    startButton.Align:=alLeft;
    startButton.Parent:=self;
    startButton.OnShowMenu:=onShowStartMenu;


    sysTray:=TXPSysTray.create(nil);
    sysTray.align:=alRight;
    sysTray.parent:=self;

    taskband:=TXPTaskBand.create(nil);
    taskband.align:=alClient;
    taskband.parent:=self;
end;

procedure TXPTaskbar.updatetask(const client: IWMClient);
begin
    taskband.updatetask(client);
end;

{ TXPSizeGrip }

constructor TXPSizeGrip.Create(AOwner: TComponent);
begin
  inherited;
  setup;
end;

destructor TXPSizeGrip.Destroy;
begin

  inherited;
end;

procedure TXPSizeGrip.setup;
var
    dir: string;
begin
    BevelOuter:=bvNone;
    dir:=getSystemInfo(XP_TASKBAR_DIR);
    bitmap.LoadFromFile(dir+'/taskbar_grip_vert.png');
    width:=bitmap.width;
end;

end.
