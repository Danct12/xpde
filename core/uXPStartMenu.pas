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
unit uXPStartMenu;

interface

uses
  SysUtils, Types, Classes,
  Variants, QTypes, QGraphics,
  QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, uXPPNG,
  uXPImageList, uCommon, Qt,
  uXPMenu, uGraphics;

type
  TXPTransPanel=class(TPanel)
  private
    procedure updateBackground; virtual;
    procedure SetParent(const Value: TWidgetControl); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
  end;

  TXPStartMenuOption=class(TXPTransPanel)
  private
    FImageList: TXPImageList;
    FImageIndex: integer;
    FCaption: string;
    FOverImageIndex: integer;
    FOver:boolean;
    FAutoSize: boolean;
    FImageAlignment: TAlignment;
    FChildMenu: TXPMenu;
    procedure SetImageList(const Value: TXPImageList);
    procedure SetImageIndex(const Value: integer);
    procedure updateBackground; override;
    procedure SetCaption(const Value: string);
    procedure SetOverImageIndex(const Value: integer);
    procedure SetAutoSize(const Value: boolean);
    procedure SetImageAlignment(const Value: TAlignment);
    procedure SetChildMenu(const Value: TXPMenu);
  public
    procedure mouseenter(AControl:TControl);override;
    procedure mouseleave(AControl:TControl);override;
    procedure showMenu(const inmediate:boolean);
    procedure click;override;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
  published
    property ImageList: TXPImageList read FImageList write SetImageList;
    property ImageIndex: integer read FImageIndex write SetImageIndex;
    property OverImageIndex: integer read FOverImageIndex write SetOverImageIndex;
    property Caption: string read FCaption write SetCaption;
    property AutoSize: boolean read FAutoSize write SetAutoSize;
    property ImageAlignment: TAlignment read FImageAlignment write SetImageAlignment;
    property ChildMenu: TXPMenu read FChildMenu write SetChildMenu;
  end;

  TStartMenu = class(TForm)
    startMenuContainer: TPanel;
    userPane: TPanel;
    logoffPane: TPanel;
    leftPanel: TPanel;
    placesList: TPanel;
    moreProg: TPanel;
    progList: TPanel;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FProgramsMenu: TXPMenu;
    FInit: boolean;
    FUserPaneBackground: TBitmap;
    FPlacesListBackground: TBitmap;
    FLogoffBackground: TBitmap;
    FMFU: TBitmap;
    FMorePrograms: TBitmap;    
    FSeparator: TBitmap;
    FImageList: TXPImageList;

    FShutDown: TXPStartMenuOption;

    FMyDocuments: TXPStartMenuOption;
    FMyRecentDocuments: TXPStartMenuOption;
    FMyPictures: TXPStartMenuOption;
    FMyMusic: TXPStartMenuOption;
    FMyComputer: TXPStartMenuOption;
    FMyHome: TXPStartMenuOption;    
    FControlPanel: TXPStartMenuOption;
    FPrinters: TXPStartMenuOption;
    FHelp: TXPStartMenuOption;
    FSearch: TXPStartMenuOption;
    FRun: TXPStartMenuOption;

    FInternet: TXPStartMenuOption;
    FEmail: TXPStartMenuOption;

    FMFU1: TXPStartMenuOption;
    FMFU2: TXPStartMenuOption;
    FMFU3: TXPStartMenuOption;
    FMFU4: TXPStartMenuOption;
    FMFU5: TXPStartMenuOption;

    FAllPrograms: TXPStartMenuOption;
    FOpenedMenu: TXPMenu;

    procedure createBitmaps;
    procedure createProgramsMenu;
    procedure destroyProgramsMenu;
    procedure destroyBitmaps;
    procedure init;
  public
    FLogOff: TXPStartMenuOption;  
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure shutdownclick(sender:TObject);
    procedure logoffclick(sender:TObject);
    procedure addSeparator(AParent:TWidgetControl);
    procedure commonClick(sender:TObject);
  end;

var
  StartMenu: TStartMenu=nil;
  iUserTile: integer;
  iShutDown: integer;
  iShutDown_over: integer;
  iLogoff: integer;
  iLogoff_over: integer;
  iMyDocuments: integer;
  iMyRecentDocuments: integer;
  iMyPictures: integer;
  iMyMusic: integer;
  iMyComputer: integer;
  iMyHome: integer;
  iControlPanel: integer;
  iPrinters: integer;
  iHelp: integer;
  iSearch: integer;
  iRun: integer;

  iInternet: integer;
  iEmail: integer;
  iMFU1: integer;
  iMFU2: integer;
  iMFU3: integer;
  iMFU4: integer;
  iMFU5: integer;

  iAllProgs: integer;

resourcestring
   sLogOff='Log Off';
   sShutDown='Shut Down';
   sMyDocuments='My Documents';
   sMyRecentDocuments='My Recent Documents';
   sMyPictures='My Pictures';
   sMyMusic='My Music';
   sMyComputer='My Computer';
   sMyHome='My Home';
   sControlPanel='Control Panel';
   sPrinters='Printers and Faxes';
   sHelp='Help and Support';
   sSearch='Search';
   sRun='Run...';

   sInternet='Internet';
   sEmail='E-mail';
   sMFU1='Gimp';
   sMFU2='AIM';
   sMFU3='Command Line';
   sMFU4='Calculator';
   sMFU5='Notepad';

   sAllProgs='All programs';

function getStartMenu:TStartMenu;

implementation

uses main, uWindowManager, uWMConsts, uTaskbar;


{$R *.xfm}

function getStartMenu:TStartMenu;
begin
    if (assigned(startMenu)) then result:=startmenu
    else begin
        startMenu:=TStartMenu.create(application);
        result:=startMenu;
    end;
end;

{ TStartMenu }

constructor TStartMenu.Create(AOwner: TComponent);
var
    st: string;
begin
  inherited;

  XPWindowManager.bypasswindow(QWidget_winId(self.handle));

  FOpenedMenu:=nil;
  FInit:=false;
  FShutDown:=TXPStartMenuOption.create(nil);
  FMyDocuments:=TXPStartMenuOption.create(nil);
  FMyRecentDocuments:=TXPStartMenuOption.create(nil);
  FMyPictures:=TXPStartMenuOption.create(nil);
  FMyMusic:=TXPStartMenuOption.create(nil);
  FMyComputer:=TXPStartMenuOption.create(nil);
  FMyHome:=TXPStartMenuOption.create(nil);  
  FControlPanel:=TXPStartMenuOption.create(nil);
  FPrinters:=TXPStartMenuOption.create(nil);
  FHelp:=TXPStartMenuOption.create(nil);
  FSearch:=TXPStartMenuOption.create(nil);
  FRun:=TXPStartMenuOption.create(nil);
  FLogOff:=TXPStartMenuOption.create(nil);

  FInternet:=TXPStartMenuOption.create(nil);
  FEmail:=TXPStartMenuOption.create(nil);
  FMFU1:=TXPStartMenuOption.create(nil);
  FMFU2:=TXPStartMenuOption.create(nil);
  FMFU3:=TXPStartMenuOption.create(nil);
  FMFU4:=TXPStartMenuOption.create(nil);
  FMFU5:=TXPStartMenuOption.create(nil);
  FAllPrograms:=TXPStartMenuOption.create(nil);                
  createBitmaps;
  createProgramsMenu;
  st:=getSystemInfo(XP_START_MENU_THEME_DIR);
  FUserPaneBackground.LoadFromFile(st+'/startmenu_user_panel.png');
  FPlacesListBackground.LoadFromFile(st+'/startmenu_places.png');
  FLogoffBackground.LoadFromFile(st+'/startmenu_logoff.png');
  FMFU.LoadFromFile(st+'/startmenu_mfu.png');
  FMorePrograms.LoadFromFile(st+'/startmenu_programs.png');
  FSeparator.LoadFromFile(st+'/startmenu_separator.png');
  iUserTile:=FImageList.add(st+'/user_tile.png');
  iShutDown:=FImageList.add(st+'/startmenu_turnoff_normal.png');
  iShutDown_over:=FImageList.add(st+'/startmenu_turnoff_over.png');
  iLogoff:=FImageList.add(st+'/startmenu_logoff_normal.png');
  iLogoff_over:=FImageList.add(st+'/startmenu_logoff_over.png');
  iMyDocuments:=FImageList.add('mydocuments.png');
  iMyRecentDocuments:=FImageList.add('myrecentdocuments.png');
  iMyPictures:=FImageList.add('mypictures.png');
  iMyMusic:=FImageList.add('mymusic.png');
  iMyComputer:=FImageList.add('mycomputer.png');
  iMyHome:=FImageList.add('myhome.png');  
  iControlPanel:=FImageList.add('controlpanel.png');
  iPrinters:=FImageList.add('printers.png');
  iHelp:=FImageList.add('help.png');
  iSearch:=FImageList.add('search.png');
  iRun:=FImageList.add('run.png');
  iAllProgs:=FImageList.add('menu_right.png');  

  FImageList.DefaultSystemDir:=XP_NORMAL_SIZE_ICON_DIR;
  iInternet:=FImageList.add('browser.png');
  iEmail:=FImageList.add('email.png');
  iMFU1:=FImageList.add('gimp.png');
  iMFU2:=FImageList.add('aim.png');
  iMFU3:=FImageList.add('command_line.png');
  iMFU4:=FImageList.add('calc.png');
  iMFU5:=FImageList.add('notepad.png');              

end;

procedure TStartMenu.createBitmaps;
begin
    FUserPaneBackground:=TBitmap.create;
    FPlacesListBackground:=TBitmap.create;
    FLogoffBackground:=TBitmap.create;
    FMFU:=TBitmap.create;
    FMorePrograms:=TBitmap.create;    
    FSeparator:=TBitmap.create;
    FImageList:=TXPImageList.create(nil);
    FImageList.DefaultSystemDir:=XP_MEDIUM_SIZE_ICON_DIR;
end;


destructor TStartMenu.Destroy;
begin
  FShutDown.free;
  FMyDocuments.free;
  FMyRecentDocuments.free;
  FMyPictures.free;
  FMyMusic.free;
  FMyComputer.free;
  FMyHome.free;
  FControlPanel.free;
  FPrinters.free;
  FHelp.free;
  FSearch.free;
  FRun.free;
  FInternet.free;
  FEmail.free;
  FMFU1.free;
  FMFU2.free;
  FMFU3.free;
  FMFU4.free;
  FMFU5.free;
  FAllPrograms.free;
  FLogOff.free;
  destroyProgramsMenu;  
  destroyBitmaps;
  inherited;
end;

procedure TStartMenu.destroyBitmaps;
begin
    FImageList.free;
    FSeparator.free;
    FMFU.free;
    FMorePrograms.free;    
    FLogoffBackground.free;
    FPlacesListBackground.free;
    FUserPaneBackground.free;
end;

procedure TStartMenu.FormShow(Sender: TObject);
begin
    if not FInit then init;
end;

procedure TStartMenu.init;
const
    u_x=64;
    u_y=20;
    f_size=16;
    f_style=[fsBold];
    m_size=11;
    m_height=32;

    p_height=44;

var
    ax: integer;
begin
    FInit:=true;
    userPane.Height:=64;
    leftPanel.Width:=191;
    placesList.width:=189;
    logoffPane.height:=38;

    ResizeBitmap(FUserPaneBackground,userPane.Bitmap,userPane.Width,userPane.height);
    FImageList.Background:=userPane.bitmap;
    FImageList.Draw(userPane.Bitmap.Canvas,3,3,iUserTile);

    userPane.Bitmap.canvas.Font.Size:=f_size;
    userPane.Bitmap.canvas.Font.style:=f_style;
    userPane.Bitmap.canvas.Font.color:=clGray;
    userPane.Bitmap.Canvas.TextOut(u_x+1,u_y+1,getSystemInfo(XP_CURRENT_USER_REAL_NAME));
    userPane.Bitmap.canvas.Font.color:=clWhite;
    userPane.Bitmap.Canvas.TextOut(u_x,u_y,getSystemInfo(XP_CURRENT_USER_REAL_NAME));


    ResizeBitmap(FPlacesListBackground,placesList.Bitmap,placesList.Width,placesList.height,2);

    ResizeBitmap(FLogoffBackground,logoffpane.Bitmap,logoffpane.Width,logoffpane.height,2);

    FShutDown.Align:=alRight;
    FShutDown.ImageList:=FImageList;
    FShutDown.ImageIndex:=iShutDown;
    FShutDown.OverImageIndex:=iShutDown_over;
    FShutDown.Caption:=sShutDown;
    FShutDown.parent:=logoffPane;
    FShutDown.OnClick:=shutdownclick;

    FLogOff.Align:=alRight;
    FLogOff.ImageList:=FImageList;
    FLogOff.ImageIndex:=iLogoff;
    FLogOff.OverImageIndex:=iLogoff_over;
    FLogOff.Caption:=sLogoff;
    FLogOff.parent:=logoffPane;
    FLogOff.width:=5;
    FLogOff.OnClick:=logoffclick;


    FRun.Align:=alTop;
    FRun.AutoSize:=False;
    FRun.height:=m_height;
    FRun.ImageList:=FImageList;
    FRun.ImageIndex:=iRun;
    FRun.OverImageIndex:=iRun;
    FRun.Font.size:=m_size;
    FRun.OnClick:=commonClick;
    FRun.Caption:=sRun;
    FRun.parent:=placesList;

    FSearch.Align:=alTop;
    FSearch.AutoSize:=False;
    FSearch.height:=m_height;
    FSearch.ImageList:=FImageList;
    FSearch.ImageIndex:=iSearch;
    FSearch.OverImageIndex:=iSearch;
    FSearch.Font.size:=m_size;
    FSearch.OnClick:=commonClick;
    FSearch.Caption:=sSearch;
    FSearch.parent:=placesList;

    FHelp.Align:=alTop;
    FHelp.AutoSize:=False;
    FHelp.height:=m_height;
    FHelp.ImageList:=FImageList;
    FHelp.ImageIndex:=iHelp;
    FHelp.OverImageIndex:=iHelp;
    FHelp.OnClick:=commonClick;
    FHelp.Font.size:=m_size;
    FHelp.Caption:=sHelp;
    FHelp.parent:=placesList;

    addSeparator(placesList);

    FPrinters.Align:=alTop;
    FPrinters.AutoSize:=False;
    FPrinters.height:=m_height;
    FPrinters.ImageList:=FImageList;
    FPrinters.ImageIndex:=iPrinters;
    FPrinters.OverImageIndex:=iPrinters;
    FPrinters.Font.size:=m_size;
    FPrinters.Caption:=sPrinters;
    FPrinters.OnClick:=commonClick;
    FPrinters.parent:=placesList;

    FControlPanel.Align:=alTop;
    FControlPanel.AutoSize:=False;
    FControlPanel.height:=m_height;
    FControlPanel.ImageList:=FImageList;
    FControlPanel.ImageIndex:=iControlPanel;
    FControlPanel.OverImageIndex:=iControlPanel;
    FControlPanel.Font.size:=m_size;
    FControlPanel.OnClick:=commonClick;
    FControlPanel.Caption:=sControlPanel;
    FControlPanel.parent:=placesList;

    addSeparator(placesList);

    FMyHome.Align:=alTop;
    FMyHome.AutoSize:=False;
    FMyHome.height:=m_height;
    FMyHome.ImageList:=FImageList;
    FMyHome.ImageIndex:=iMyHome;
    FMyHome.OverImageIndex:=iMyHome;
    FMyHome.Font.Style:=f_style;
    FMyHome.Font.size:=m_size;
    FMyHome.OnClick:=commonClick;
    FMyHome.Caption:=sMyHome;
    FMyHome.parent:=placesList;

    FMyComputer.Align:=alTop;
    FMyComputer.AutoSize:=False;
    FMyComputer.height:=m_height;
    FMyComputer.ImageList:=FImageList;
    FMyComputer.ImageIndex:=iMyComputer;
    FMyComputer.OverImageIndex:=iMyComputer;
    FMyComputer.Font.Style:=f_style;
    FMyComputer.Font.size:=m_size;
    FMyComputer.OnClick:=commonClick;
    FMyComputer.Caption:=sMyComputer;
    FMyComputer.parent:=placesList;

    FMyMusic.Align:=alTop;
    FMyMusic.AutoSize:=False;
    FMyMusic.height:=m_height;
    FMyMusic.ImageList:=FImageList;
    FMyMusic.ImageIndex:=iMyMusic;
    FMyMusic.OverImageIndex:=iMyMusic;
    FMyMusic.Font.Style:=f_style;
    FMyMusic.Font.size:=m_size;
    FMyMusic.OnClick:=commonClick;
    FMyMusic.Caption:=sMyMusic;
    FMyMusic.parent:=placesList;

    FMyPictures.Align:=alTop;
    FMyPictures.AutoSize:=False;
    FMyPictures.height:=m_height;
    FMyPictures.ImageList:=FImageList;
    FMyPictures.ImageIndex:=iMyPictures;
    FMyPictures.OverImageIndex:=iMyPictures;
    FMyPictures.Font.Style:=f_style;
    FMyPictures.Font.size:=m_size;
    FMyPictures.OnClick:=commonClick;
    FMyPictures.Caption:=sMyPictures;
    FMyPictures.parent:=placesList;

    FMyRecentDocuments.Align:=alTop;
    FMyRecentDocuments.AutoSize:=False;
    FMyRecentDocuments.height:=m_height;
    FMyRecentDocuments.ImageList:=FImageList;
    FMyRecentDocuments.ImageIndex:=iMyRecentDocuments;
    FMyRecentDocuments.OverImageIndex:=iMyRecentDocuments;
    FMyRecentDocuments.Font.Style:=f_style;
    FMyRecentDocuments.Font.size:=m_size;
    FMyRecentDocuments.OnClick:=commonClick;
    FMyRecentDocuments.Caption:=sMyRecentDocuments;
    FMyRecentDocuments.parent:=placesList;

    FMyDocuments.Align:=alTop;
    FMyDocuments.AutoSize:=False;
    FMyDocuments.height:=m_height;
    FMyDocuments.ImageList:=FImageList;
    FMyDocuments.ImageIndex:=iMyDocuments;
    FMyDocuments.OverImageIndex:=iMyDocuments;
    FMyDocuments.Font.Style:=f_style;
    FMyDocuments.Font.size:=m_size;
    FMyDocuments.OnClick:=commonClick;
    FMyDocuments.Caption:=sMyDocuments;
    FMyDocuments.parent:=placesList;


    ResizeBitmap(FMFU,progList.Bitmap,progList.Width,progList.height,2);
    ResizeBitmap(FMorePrograms,moreProg.Bitmap,moreProg.Width,moreProg.height,2);

    FMFU5.Align:=alTop;
    FMFU5.AutoSize:=False;
    FMFU5.height:=p_height;
    FMFU5.ImageList:=FImageList;
    FMFU5.ImageIndex:=iMFU5;
    FMFU5.OverImageIndex:=iMFU5;
    FMFU5.Font.size:=m_size;
    FMFU5.Caption:=sMFU5;
    FMFU5.OnClick:=commonClick;
    FMFU5.parent:=progList;

    FMFU4.Align:=alTop;
    FMFU4.AutoSize:=False;
    FMFU4.height:=p_height;
    FMFU4.ImageList:=FImageList;
    FMFU4.ImageIndex:=iMFU4;
    FMFU4.OverImageIndex:=iMFU4;
    FMFU4.Font.size:=m_size;
    FMFU4.Caption:=sMFU4;
    FMFU4.OnClick:=commonClick;
    FMFU4.parent:=progList;

    FMFU3.Align:=alTop;
    FMFU3.AutoSize:=False;
    FMFU3.height:=p_height;
    FMFU3.ImageList:=FImageList;
    FMFU3.ImageIndex:=iMFU3;
    FMFU3.OverImageIndex:=iMFU3;
    FMFU3.Font.size:=m_size;
    FMFU3.Caption:=sMFU3;
    FMFU3.OnClick:=commonClick;
    FMFU3.parent:=progList;

    FMFU2.Align:=alTop;
    FMFU2.AutoSize:=False;
    FMFU2.height:=p_height;
    FMFU2.ImageList:=FImageList;
    FMFU2.ImageIndex:=iMFU2;
    FMFU2.OverImageIndex:=iMFU2;
    FMFU2.Font.size:=m_size;
    FMFU2.Caption:=sMFU2;
    FMFU2.OnClick:=commonClick;
    FMFU2.parent:=progList;

    FMFU1.Align:=alTop;
    FMFU1.AutoSize:=False;
    FMFU1.height:=p_height;
    FMFU1.ImageList:=FImageList;
    FMFU1.ImageIndex:=iMFU1;
    FMFU1.OverImageIndex:=iMFU1;
    FMFU1.Font.size:=m_size;
    FMFU1.Caption:=sMFU1;
    FMFU1.OnClick:=commonClick;
    FMFU1.parent:=progList;

    addSeparator(proglist);

    FEmail.Align:=alTop;
    FEmail.AutoSize:=False;
    FEmail.height:=p_height;
    FEmail.ImageList:=FImageList;
    FEmail.ImageIndex:=iEmail;
    FEmail.OverImageIndex:=iEmail;
    FEmail.Font.size:=m_size;
    FEmail.Font.style:=f_style;
    FEmail.Caption:=sEmail;
    FEmail.OnClick:=commonClick;
    FEmail.parent:=progList;

    FInternet.Align:=alTop;
    FInternet.AutoSize:=False;
    FInternet.height:=p_height;
    FInternet.ImageList:=FImageList;
    FInternet.ImageIndex:=iInternet;
    FInternet.OverImageIndex:=iInternet;
    FInternet.Font.size:=m_size;
    FInternet.Font.style:=f_style;    
    FInternet.Caption:=sInternet;
    FInternet.OnClick:=commonClick;
    FInternet.parent:=progList;

    FAllPrograms.Align:=alTop;
    FAllPrograms.AutoSize:=False;
    FAllPrograms.height:=p_height;
    FAllPrograms.ImageList:=FImageList;
    FAllPrograms.ImageAlignment:=taRightJustify;
    FAllPrograms.Alignment:=taCenter;
    FAllPrograms.ImageIndex:=iAllProgs;
    FAllPrograms.OverImageIndex:=iAllProgs;
    FAllPrograms.Font.size:=m_size;
    FAllPrograms.Font.style:=f_style;    
    FAllPrograms.Caption:=sAllProgs;
    FAllPrograms.OnClick:=commonClick;
    FAllPrograms.ChildMenu:=FProgramsMenu;
    FAllPrograms.parent:=moreProg;

    addSeparator(moreProg);


end;

procedure TStartMenu.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    taskbar.ataskbar.startButton.release;
    if assigned(fopenedmenu) then fopenedmenu.closepopup(true);
end;

procedure TStartMenu.logoffclick(sender: TObject);
begin
    application.terminate;
end;

procedure TStartMenu.shutdownclick(sender: TObject);
begin
    application.terminate;
end;

procedure TStartMenu.addSeparator(AParent: TWidgetControl);
var
    sep: TPanel;
begin
    sep:=TPanel.create(self);
    sep.BevelOuter:=bvNone;
    sep.Align:=alTop;
    sep.Bitmap.assign(FSeparator);
    sep.height:=sep.bitmap.height;
    sep.parent:=AParent;
    ResizeBitmap(FSeparator,sep.Bitmap,sep.Width,sep.height,0);    
end;

procedure TStartMenu.commonClick(sender: TObject);
begin
    close;
end;

procedure TStartMenu.createProgramsMenu;
begin
    FProgramsMenu:=TXPMenu.create(nil);
    FProgramsMenu.Directory:=getSystemInfo(XP_START_MENU_PROGRAMS_DIR);
end;

procedure TStartMenu.destroyProgramsMenu;
begin
    FProgramsMenu.free;
end;

{ TXPTransPanel }

constructor TXPTransPanel.Create(AOwner: TComponent);
begin
  inherited;
  Alignment:=taLeftJustify;
  BevelOuter:=bvNone;
end;

destructor TXPTransPanel.Destroy;
begin

  inherited;
end;

procedure TXPTransPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  updatebackground;
end;

procedure TXPTransPanel.SetParent(const Value: TWidgetControl);
begin
  inherited;
  updatebackground;
  if (not (csDestroying in ComponentState)) then begin
    XPWindowManager.bypasswindow(QWidget_winId(self.handle));
  end;
end;

procedure TXPTransPanel.updateBackground;
var
    sourcecanvas: TCanvas;
begin
    if (not (csDestroying in componentstate)) then begin
        if assigned(parent) then begin
            if (parent is TPanel) then begin
                bitmap.Width:=self.width;
                bitmap.height:=self.height;
                sourcecanvas:=(parent as TPanel).bitmap.canvas;
                bitmap.Canvas.CopyRect(clientrect,sourcecanvas,boundsrect);
            end;
        end;
    end;
end;


{ TXPStartMenuOption }

procedure TXPStartMenuOption.click;
begin
    if (assigned(FChildMenu)) then begin
        showmenu(true);
    end
    else inherited;
end;

constructor TXPStartMenuOption.Create(AOwner: TComponent);
begin
  inherited;
  FChildMenu:=nil;
  FImageAlignment:=taLeftJustify;
  FAutosize:=true;
  FCaption:='';
  FImageList:=nil;
  FImageIndex:=-1;
  FOverImageIndex:=-1;
end;

destructor TXPStartMenuOption.Destroy;
begin

  inherited;
end;

procedure TXPStartMenuOption.mouseenter(AControl: TControl);
begin
  inherited;
  if assigned(getStartMenu.FOpenedMenu) then begin
    getStartMenu.FOpenedMenu.closepopup(false);
  end;
  fover:=true;
  updatebackground;
  showMenu(false);
end;

procedure TXPStartMenuOption.mouseleave(AControl: TControl);
begin
  inherited;
  fover:=false;
  updatebackground;
  if assigned(FChildMenu) then FChildMenu.closepopup(false);
end;

procedure TXPStartMenuOption.SetAutoSize(const Value: boolean);
begin
    if (FAutosize<>Value) then begin
        FAutoSize := Value;
        updateBackground;
    end;
end;

procedure TXPStartMenuOption.SetCaption(const Value: string);
begin
    if (Value<>FCaption) then begin
        FCaption := Value;
        updateBackground;
    end;
end;

procedure TXPStartMenuOption.SetChildMenu(const Value: TXPMenu);
begin
  FChildMenu := Value;
end;

procedure TXPStartMenuOption.SetImageAlignment(const Value: TAlignment);
begin
    if (Value<>FImageAlignment) then begin
        FImageAlignment := Value;
    end;
end;

procedure TXPStartMenuOption.SetImageIndex(const Value: integer);
begin
    if (value<>FImageIndex) then begin
        FImageIndex := Value;
        updateBackground;
    end;
end;

procedure TXPStartMenuOption.SetImageList(const Value: TXPImageList);
begin
    if (value<>FImageList) then begin
        FImageList := Value;
        updateBackground;
    end;
end;

procedure TXPStartMenuOption.SetOverImageIndex(const Value: integer);
begin
    if Value<>FOverImageIndex then begin
        FOverImageIndex := Value;
    end;
end;

procedure TXPStartMenuOption.showMenu(const inmediate: boolean);
var
    p: TPoint;
begin
    if (assigned(FChildMenu)) then begin
        p.x:=BoundsRect.right;
        p.y:=boundsrect.bottom;
        p:=parent.ClientToScreen(p);
        { TODO : add a property to choose the popupmenu orientation }
        p.y:=p.y-FChildMenu.height-6;
        p.x:=p.x-6;
        FChildMenu.popup(p.x,p.y, inmediate);
        getStartMenu.FOpenedMenu:=FChildMenu;
    end;
end;

procedure TXPStartMenuOption.updateBackground;
var
    sRect: TRect;
    oldBack: TBitmap;
    tx, ty: integer;
    ix: integer;
    gy: integer;
begin
    if (FAutoSize) then begin
        if (not (csDestroying in componentstate)) then begin
            if assigned(parent) then begin
                if (parent is TPanel) then begin
                    if (assigned(FImageList)) then begin
                        if (FImageIndex<>-1) and (FOverImageIndex<>-1) then begin
                            width:=canvas.TextWidth(FCaption)+40;
                        end;
                    end;
                end;
            end;
        end;
    end;
  inherited;
    if (not (csDestroying in componentstate)) then begin
        if assigned(parent) then begin
            if (parent is TPanel) then begin
                tx:=0;
                if (assigned(FImageList)) then begin
                    if (FImageIndex<>-1) and (FOverImageIndex<>-1) then begin
                        oldBack:=FImageList.Background;
                        try
                            FImageList.Background:=self.bitmap;
                            if fover then begin
                                if (FImageAlignment=taLeftJustify) then ix:=4
                                else if (FImageAlignment=taRightJustify) then begin
                                    ix:=width-FImageList.getbitmapwidth(FOverImageIndex)-4;
                                end;
                                sRect:=clientRect;
                                sRect.left:=sRect.left+1;
                                sRect.top:=sRect.top+4;
                                sRect.bottom:=sRect.bottom-4;
                                sRect.right:=sRect.right-2;

                                bitmap.Canvas.brush.color:=clHighLight;
                                bitmap.Canvas.pen.color:=clHighLight;
                                bitmap.Canvas.Rectangle(sRect);
                                gy:=(height-FImageList.getbitmapheight(FOverImageIndex)) div 2;
                                FImageList.Draw(bitmap.canvas,ix,gy,FOverImageIndex,false,0,false);
                                tx:=FImageList.getbitmapwidth(FOverImageIndex)+8;
                            end
                            else begin
                                if (FImageAlignment=taLeftJustify) then ix:=4
                                else if (FImageAlignment=taRightJustify) then begin
                                    ix:=width-FImageList.getbitmapwidth(FImageIndex)-4;
                                end;
                                gy:=(height-FImageList.getbitmapheight(FImageIndex)) div 2;
                                FImageList.Draw(bitmap.canvas,ix,gy,FImageIndex,false,0,false);
                                tx:=FImageList.getbitmapwidth(FImageIndex)+8;
                            end;
                        finally
                            FImageList.Background:=oldBack;
                        end;
                    end
                    else tx:=32;
                end;
                if (Alignment=taCenter) then begin
                    tx:=(width-bitmap.canvas.TextWidth(FCaption)) div 2;
                end;
                ty:=(height - bitmap.canvas.textheight(FCaption)) div 2;
                bitmap.canvas.Font.assign(self.font);
                bitmap.Canvas.Font.Color:=clWhite;
                bitmap.canvas.TextOut(tx,ty,FCaption);
            end;
        end;
    end;
end;

end.
