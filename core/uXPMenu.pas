unit uXPMenu;

interface

uses
  SysUtils, Types, Classes,
  Variants, QTypes, QGraphics,
  QControls, QForms, QDialogs,
  QStdCtrls, Qt, uGraphics,
  iniFiles, QExtCtrls,
  uCommon, uXPImageList;

type
    TXPMenuItems=class;

    TXPMenuItem=class(TObject)
    private
        FCaption: string;
        FItems: TXPMenuItems;
        FImageIndex: integer;
        FPath: string;
        FIsFolder: boolean;
        FItemIndex: integer;
        procedure SetCaption(const Value: string);
        procedure SetImageIndex(const Value: integer);
        procedure SetPath(const Value: string);
        procedure SetIsFolder(const Value: boolean);
        procedure SetItemIndex(const Value: integer);
    public
        constructor Create;virtual;
        destructor Destroy;override;
        property Caption: string read FCaption write SetCaption;
        property ImageIndex: integer read FImageIndex write SetImageIndex;
        property Items: TXPMenuItems read FItems;
        property ItemIndex: integer read FItemIndex write SetItemIndex;
        property Path: string read FPath write SetPath;
        property IsFolder: boolean read FIsFolder write SetIsFolder;
    end;

    TXPMenuItems=class(TList)
    private
        FImageList: TXPImageList;
        FInUpdate:boolean;
        FOnItemsChanged: TNotifyEvent;
        FItemsDirectory: string;
        procedure Notify(Ptr: Pointer; Action: TListNotification); override;
        procedure SetOnItemsChanged(const Value: TNotifyEvent);
        procedure SetItemsDirectory(const Value: string);
        procedure populateItems;
    public
        procedure beginupdate;
        procedure endupdate;
        function additem(const caption:string;const ImageIndex:integer=-1): TXPMenuItem;
        constructor Create;virtual;
        destructor Destroy;override;
        property OnItemsChanged: TNotifyEvent read FOnItemsChanged write SetOnItemsChanged;
        property ItemsDirectory: string read FItemsDirectory write SetItemsDirectory;
    end;

  TXPMenu = class(TForm)
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    FTextOffset: integer;
    FBackBitmap: TBitmap;
    FItems: TXPMenuItems;
    FItemHeight: integer;
    FBackground: TBitmap;
    FChild: TXPMenu;
    FSelectedItem: TXPMenuItem;
    FTimer: TTimer;
    FCloseTimer: TTimer;
    FDelayedPath: string;
    allowshowmenu:boolean;
    procedure paintItem(const itemindex:integer);
    procedure SetItemHeight(const Value: integer);
    procedure ItemsChanged(sender:TObject);
    procedure SetDirectory(const Value: string);
    function getDirectory: string;
    procedure createChild;
    procedure SetSelectedItem(const Value: TXPMenuItem);
    procedure OnShowTimer(sender:TObject);
    procedure OnCloseTimer(sender:TObject);
    { Private declarations }
  public
    { Public declarations }
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);override;
    procedure mouseenter(AControl:TControl);override;
    procedure mouseleave(AControl:TControl);override;
    function itemAtPos(const pt: TPoint):TXPMenuItem;
    procedure showmenu;
    procedure hidemenu;
    procedure popup(const x,y:integer;const inmediate:boolean=true;const delayedpath:string='');
    procedure closepopup(const inmediate:boolean=true);
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property Items:TXPMenuItems read FItems;
    property ItemHeight: integer read FItemHeight write SetItemHeight;
    property SelectedItem: TXPMenuItem read FSelectedItem write SetSelectedItem;
    property Directory: string read getDirectory write SetDirectory;
  end;

var
  XPMenu: TXPMenu;

implementation

uses uWindowManager, uXPStartMenu;

{$R *.xfm}

{ TXPMenuItem }

constructor TXPMenuItem.Create;
begin
  inherited;
  FItemIndex:=-1;
  FIsFolder:=false;
  FPath:='';
  FItems:=TXPMenuItems.create;
  FImageIndex:=-1;
end;

destructor TXPMenuItem.Destroy;
begin
  FItems.free;
  inherited;
end;


procedure TXPMenuItem.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TXPMenuItem.SetImageIndex(const Value: integer);
begin
  FImageIndex := Value;
end;


procedure TXPMenuItem.SetIsFolder(const Value: boolean);
begin
  FIsFolder := Value;
end;

procedure TXPMenuItem.SetItemIndex(const Value: integer);
begin
  FItemIndex := Value;
end;

procedure TXPMenuItem.SetPath(const Value: string);
begin
  FPath := Value;
end;

{ TXPMenu }

constructor TXPMenu.Create(AOwner: TComponent);
begin
  inherited;
  FDelayedPath:='';
  FTimer:=TTimer.create(nil);
  FTimer.enabled:=false;
  FTimer.Interval:=500;

  FCloseTimer:=TTimer.create(nil);
  FCloseTimer.enabled:=false;
  FCloseTimer.Interval:=500;

  FTimer.OnTimer:=OnShowTimer;
  FCloseTimer.OnTimer:=OnCloseTimer;

  FTextOffset:=0;
  FBackBitmap:=TBitmap.create;
  FChild:=nil;
  FSelectedItem:=nil;
  FBackground:=TBitmap.create;
  FItems:=TXPMenuItems.create;
  FItems.OnItemsChanged:=ItemsChanged;
  FItemHeight:=24;
  FBackground.LoadFromFile(getSystemInfo(XP_START_MENU_THEME_DIR)+'/startmenu_options_background.png');
  ResizeBitmap(FBackground,Bitmap,Width,height,4);
end;

destructor TXPMenu.Destroy;
begin
  FBackBitmap.free;
  FBackground.free;
  FItems.free;
  if (assigned(FChild)) then FChild.free;
  inherited;
end;

function TXPMenu.getDirectory: string;
begin
    result:=FItems.ItemsDirectory;
end;

procedure TXPMenu.ItemsChanged(sender: TObject);
var
    new_width: integer;
    i: integer;
    item: TXPMenuItem;
    toadd: integer;
begin
    if (not (csDestroying in ComponentState)) then begin
        FTextOffset:=6;
        new_width:=50;
        for i:=0 to FItems.count-1 do begin
            toadd:=0;
            item:=FItems[I];
            //If it's a folder
            if (item.isfolder) then begin
                toadd:=24;
            end;
            if (item.imageindex<>-1) then begin
                toadd:=toadd+24;
                FTextOffset:=30;
            end;
            new_width:=max(new_width,bitmap.canvas.TextWidth(item.Caption)+16+FTextOffset+toadd);
        end;
        width:=new_width;
        height:=FItemHeight*FItems.Count;
        ResizeBitmap(FBackground,Bitmap,Width,height,4);
        fbackbitmap.assign(bitmap);
        bitmap.Canvas.Font.Color:=clWhite;

        for i:=0 to FItems.count-1 do begin
            paintItem(i);
        end;
    end;
end;


procedure TXPMenu.popup(const x, y: integer;const inmediate:boolean=true;const delayedpath:string='');
begin
    left:=x;
    top:=y;
    if (inmediate) then showmenu
    else begin
        allowshowmenu:=false;
        FTimer.Enabled:=false;
        FDelayedPath:=delayedpath;
        FCloseTimer.Enabled:=false;
        FTimer.Enabled:=true;
    end;
end;

procedure TXPMenu.SetDirectory(const Value: string);
begin
    FItems.ItemsDirectory:=value;
end;

procedure TXPMenu.SetItemHeight(const Value: integer);
begin
  FItemHeight := Value;
end;

function TXPMenu.itemAtPos(const pt: TPoint): TXPMenuItem;
var
    it_index: integer;
begin
    result:=nil;
    it_index:=pt.y div FItemHeight;
    if (it_index>=0) and (it_index<FItems.count) then result:=FItems[it_index];
end;

procedure TXPMenu.createChild;
begin
    if not (assigned(FChild)) then begin
        FChild:=TXPMenu.create(nil);
    end;
end;

procedure TXPMenu.SetSelectedItem(const Value: TXPMenuItem);
var
    oldindex: integer;
begin
    if (value<>FSelectedItem) then begin
          oldindex:=-1;
          if assigned(FSelectedItem) then begin
            oldindex:=FSelectedItem.ItemIndex;
          end;
          FSelectedItem := Value;
          if (oldindex<>-1) then begin
            paintItem(oldindex);
          end;
          if assigned(FSelectedItem) then begin
            paintItem(FSelectedItem.itemindex);
          end;
    end;
end;

procedure TXPMenu.paintItem(const itemindex:integer);
var
    y:integer;
    item: TXPMenuItem;
    d:integer;
    cRect: TRect;
begin
        //Draw here the bitmap
        y:=itemindex*FItemHeight;
        FItems.FImageList.Background:=fbackbitmap;

        item:=FItems[itemindex];

        cRect:=Rect(2,y+2,width-2,y+FItemHeight-2);
        if (item=FSelectedItem) then begin
            bitmap.Canvas.Brush.Color:=clHighLight;
            bitmap.Canvas.pen.Color:=clHighLight;
            bitmap.canvas.Rectangle(cRect);
            FItems.FImageList.Background:=bitmap;
        end
        else begin
            bitmap.Canvas.CopyRect(cRect,fbackbitmap.canvas,cRect);
        end;

        d:=(fitemheight-bitmap.Canvas.textheight(item.caption)) div 2;

        bitmap.Canvas.TextOut(FTextOffset,y+d,item.caption);

        if (item.ImageIndex<>-1) then begin
            FItems.FImageList.Draw(bitmap.canvas,8,y+4,item.ImageIndex,false,0,false);

            if (item.isFolder) then begin
                FItems.FImageList.Draw(bitmap.canvas,width-12,y+8,item.imageindex+1,false,0,false);
            end;
        end;
end;

procedure TXPMenu.OnShowTimer(sender: TObject);
begin
    allowshowmenu:=true;
    FTimer.enabled:=false;
    Directory:=FDelayedPath;
    if (top+height) > screen.height then top:=screen.height-height;
    if (allowshowmenu) then showmenu;
end;

procedure TXPMenu.showmenu;
begin
    if (not visible) then begin
        SelectedItem:=nil;
        ItemsChanged(nil);
        show;
    end;
end;


procedure TXPMenu.hidemenu;
begin
   if (assigned(FChild)) then FChild.closepopup(true);
   FTimer.enabled:=false;
   FCloseTimer.enabled:=false;
   hide;
end;

procedure TXPMenu.closepopup(const inmediate: boolean);
begin
   if (inmediate) then hidemenu
   else begin
     FTimer.enabled:=false;
     FCloseTimer.enabled:=true;
   end;
end;

procedure TXPMenu.OnCloseTimer(sender: TObject);
begin
    hidemenu;
    FCloseTimer.enabled:=false;
end;

procedure TXPMenu.mouseenter(AControl: TControl);
begin
  inherited;
end;

procedure TXPMenu.mouseleave(AControl: TControl);
begin
  inherited;
end;

procedure TXPMenu.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

{ TXPMenuItems }

function TXPMenuItems.additem(const caption: string; const ImageIndex: integer=-1): TXPMenuItem;
begin
    result:=TXPMenuItem.create;
    result.caption:=caption;
    result.imageindex:=imageindex;
    result.ItemIndex:=add(result);
end;

procedure TXPMenuItems.beginupdate;
begin
    FInUpdate:=true;
end;

constructor TXPMenuItems.Create;
begin
    inherited;
    FImageList:=TXPImageList.create(nil);
    FImageList.DefaultSystemDir:=XP_SMALL_SIZE_ICON_DIR;
    FImageList.add('programs_folder.png');
    FImageList.add('menu_subitems.png');
    FItemsDirectory:='';
    FOnItemsChanged:=nil;
    FInUpdate:=false;
end;

destructor TXPMenuItems.Destroy;
begin
  FImageList.free;
  inherited;
end;

procedure TXPMenuItems.endupdate;
begin
    FInUpdate:=false;
    if assigned(FOnItemsChanged) then FOnItemsChanged(self);
end;

procedure TXPMenuItems.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if (assigned(FOnItemsChanged) and (not FInUpdate)) then FOnItemsChanged(self);
end;

procedure TXPMenuItems.populateItems;
var
    searchRec: TSearchRec;
    item: TXPMenuItem;
    ini: TIniFile;
    image: string;
    files: TStringList;
    folders: TStringList;
    i: integer;
    path: string;
begin
  if (FItemsDirectory<>'') then begin
    if (DirectoryExists(FItemsDirectory)) then begin
        Clear;
        beginupdate;
        files:=TStringList.create;
        files.sorted:=true;
        folders:=TStringList.create;
        folders.Sorted:=true;
        try
            if FindFirst(IncludeTrailingPathDelimiter(FItemsDirectory) + '*', faAnyFile, SearchRec) = 0 then begin
                { TODO : Sort items alphabetically }
                repeat
                    if ((SearchRec.Name <> '..') and (SearchRec.Name <> '.')) then begin
                        { TODO : Process .lnk files to get caption and icon }
                        { TODO : Sort these items alphabetically }
                        { TODO : Get the real item caption }
                        if ((searchRec.Attr and faDirectory)=faDirectory) then begin
                            folders.add(searchrec.PathOnly+searchrec.name);
                        end
                        else begin
                            files.add(searchrec.PathOnly+searchrec.name);
                        end;
                    end;
                    application.processmessages;
                until FindNext(SearchRec) <> 0;
                FindClose(SearchRec);
            end;
            if (files.count=0) and (folders.count=0) then begin
                clear;
                additem('(empty)');
            end
            else begin
                for i:=0 to folders.count-1 do begin
                    path:=folders[i];
                    item:=additem(extractfilename(changefileext(path,'')));
                    item.Path:=path;
                    item.isfolder:=true;
                    item.imageindex:=0;
                end;

                for i:=0 to files.count-1 do begin
                    path:=files[i];
                    item:=additem(extractfilename(changefileext(path,'')));
                    if (ansilowercase(ExtractFileExt(path))='.lnk') then begin
                        ini:=TIniFile.create(path);
                        try
                            image:=ini.ReadString('Shortcut','Icon','');
                            if (image<>'') then item.imageindex:=FImageList.Add(image);
                        finally
                            ini.free;
                        end;
                    end;
                    item.Path:=path;
                    item.isfolder:=false;
                end;
            end;
        finally
            folders.free;
            files.free;
            endupdate;
        end;
    end;
  end;
end;

procedure TXPMenuItems.SetItemsDirectory(const Value: string);
begin
    if (FItemsDirectory<>Value) then begin
        FItemsDirectory := Value;
        populateItems;
    end;
end;

procedure TXPMenuItems.SetOnItemsChanged(const Value: TNotifyEvent);
begin
  FOnItemsChanged := Value;
end;

procedure TXPMenu.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
    item: TXPMenuItem;
    pt: TPoint;
begin
    FCloseTimer.enabled:=false;
    pt:=point(x,y);
    item:=itematpos(pt);
    selecteditem:=item;
    pt.y:=(pt.Y div fitemheight)*fitemheight;
    pt.x:=width;
    pt:=ClientToScreen(pt);
    if (assigned(item)) then begin
        if (item.IsFolder) then begin
            createChild;
            FChild.FSelectedItem:=nil;
            FChild.Directory:=item.Path;
            if (pt.y+fchild.height) > screen.height then pt.y:=screen.height-fchild.height;
            FChild.popup(pt.x,pt.y,true);
        end
        else begin
            ShellExecute(item.Path);
            getstartmenu.close;
        end;
    end;
end;

procedure TXPMenu.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
    pt: TPoint;
    old_sel: TXPMenuItem;
begin
    FCloseTimer.enabled:=false;
    pt:=point(x,y);
    old_sel:=selecteditem;
    selecteditem:=itematpos(pt);
    pt.y:=(pt.Y div fitemheight)*fitemheight;
    pt.x:=width;
    pt:=ClientToScreen(pt);
    if (selecteditem<>old_sel) then begin
        if (assigned(FChild)) then begin
            FChild.closepopup(true);
            FChild.FTimer.Enabled:=false;
            FChild.FCloseTimer.Enabled:=false;
        end;
    end;
    if (assigned(selecteditem)) then begin
        if (selecteditem.isFolder) then begin
            createChild;
            FChild.FSelectedItem:=nil;
            if (selecteditem<>old_sel) then begin
                FChild.closepopup(true);
                FChild.popup(pt.x,pt.y, false, selecteditem.path);
            end;
        end
        else begin
            //Do nothing
        end;
    end;
end;

procedure TXPMenu.FormShow(Sender: TObject);
begin
  { TODO : Fix this and find the right handle to bypass }
  XPWindowManager.bypasswindow(qwidget_winid(self.handle)-1);
end;

end.
