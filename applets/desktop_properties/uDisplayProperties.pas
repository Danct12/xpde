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
unit uDisplayProperties;

interface

uses
  SysUtils, Types, Classes,
  QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QExtCtrls,
  QComCtrls,Qt, QImgList,
  QButtons, uXPColorSelector,
  uRegistry, uXPIPC, uCommon,
  uGraphics;

type
  TDisplayPropertiesDlg = class(TForm)
        Button15:TButton;
        Label19:TLabel;
    cbPosition: TComboBox;
    lbPosition: TLabel;
    btnBrowse: TButton;
    lvPictures: TListView;
        Label17:TLabel;
        Label16:TLabel;
        Button13:TButton;
        Image4:TImage;
        Panel4:TPanel;
        GroupBox4:TGroupBox;
        CheckBox1:TCheckBox;
        Label15:TLabel;
        Edit1:TEdit;
        Label14:TLabel;
        Button12:TButton;
        Button11:TButton;
        ComboBox7:TComboBox;
        GroupBox3:TGroupBox;
        Image3:TImage;
        Panel3:TPanel;
        Button10:TButton;
        Button9:TButton;
        ComboBox6:TComboBox;
        Label13:TLabel;
        ComboBox5:TComboBox;
        Label12:TLabel;
        ComboBox4:TComboBox;
        Label11:TLabel;
        Button8:TButton;
        Button7:TButton;
        Image2:TImage;
        Panel2:TPanel;
        ComboBox3:TComboBox;
        GroupBox2:TGroupBox;
        Label10:TLabel;
        Label9:TLabel;
        Label8:TLabel;
        TrackBar1:TTrackBar;
        GroupBox1:TGroupBox;
        Label7:TLabel;
        ComboBox2:TComboBox;
        Label6:TLabel;
        Label5:TLabel;
        Label4:TLabel;
        Image1:TImage;
        Panel1:TPanel;
        Label3:TLabel;
        Button6:TButton;
        Button5:TButton;
        ComboBox1:TComboBox;
        Label2:TLabel;
        Label1:TLabel;
        TabSheet5:TTabSheet;
        TabSheet4:TTabSheet;
        TabSheet3:TTabSheet;
        TabSheet2:TTabSheet;
        PageControl1:TPageControl;
        Button4:TButton;
    btnApply: TButton;
    btnCancel: TButton;
    btnOk: TButton;
        TabSheet1:TTabSheet;
    Image5: TImage;
    imgList: TImageList;
    csBackground: TXPColorSelector;
    pnColor: TPanel;
    imgBack: TImage;
    odPictures: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure lvPicturesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnOkClick(Sender: TObject);
    procedure lvPicturesCustomDrawItem(Sender: TCustomViewControl;
      Item: TCustomViewItem; Canvas: TCanvas; const Rect: TRect;
      State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure csBackgroundChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbPositionChange(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    selectenabled: boolean;
    lastimage:string;
    function addimagetolist(const filename:string):TListItem;
    procedure loadPictureDir;
    procedure loadImage(const filename:string);
    procedure storeproperties;
  end;

type
    TTilemode=(tmCenter, tmTile, tmStretch);
    TDesktop=record
        color:TColor;
        method: integer;
        desktopimage:string;
    end;

var
  DisplayPropertiesDlg: TDisplayPropertiesDlg=nil;
  desktop:TDesktop;

resourcestring
    sNone='(None)';

procedure customizeDesktop;

implementation

{$R *.xfm}

procedure customizeDesktop;
begin
    //Creates the dialog
    if not assigned(DisplayPropertiesDlg) then begin
        Application.CreateForm(TDisplayPropertiesDlg,DisplayPropertiesDlg);
    end;
end;


procedure TDisplayPropertiesDlg.FormCreate(Sender: TObject);
var
    reg:TRegistry;
    col: string;
    path:string;
    method:integer;
    b:TBitmap;
    wallpapers_dir: string;
begin
    //Load the images
    image5.Picture.LoadFromFile(getSystemInfo(XP_MISC_DIR)+'/desktoppropertiesmonitor.png');
    b:=TBitmap.create;
    try
        b.LoadFromFile(getSystemInfo(XP_MISC_DIR)+'/none.png');
        b.width:=imgList.Width;
        b.height:=imgList.height;
        imgList.AddMasked(b,clFuchsia);

        b.LoadFromFile(getSystemInfo(XP_MISC_DIR)+'/bmp.png');
        b.width:=imgList.Width;
        b.height:=imgList.height;
        imgList.AddMasked(b,clFuchsia);

        b.LoadFromFile(getSystemInfo(XP_MISC_DIR)+'/jpg.png');
        b.width:=imgList.Width;
        b.height:=imgList.height;
        imgList.Add(b,b);

        b.LoadFromFile(getSystemInfo(XP_MISC_DIR)+'/png.png');
        b.width:=imgList.Width;
        b.height:=imgList.height;
        imgList.Add(b,b);
    finally
        b.free;
    end;

    //Load the configurations
    selectenabled:=false;
    try
        PageControl1.ActivePageIndex:=1;

        reg:=TRegistry.create;
        try
            if reg.OpenKey('Software/XPde/Desktop/Background',false) then begin
                if reg.ValueExists('Color') then begin
                    col:=reg.ReadString('Color');
                    desktop.color:=stringtocolor(col);
                end
                else desktop.color:=clHighLight;
            end
            else desktop.color:=clHighLight;
        finally
            reg.free;
        end;

        reg:=TRegistry.create;
        try
            if reg.OpenKey('Software/XPde/Desktop/Background',false) then begin
                path:=reg.ReadString('Image');
                method:=reg.ReadInteger('Method');
                if path='none' then begin
                    desktop.desktopimage:='';
                end
                else begin
                    desktop.desktopimage:=path;
                    desktop.method:=method;
                end;
            end
            else begin
                wallpapers_dir:=getSystemInfo(XP_WALLPAPERS_DIR);
                if (fileexists(wallpapers_dir+'/default.png')) then begin
                    desktop.desktopimage:=wallpapers_dir+'/default.png';
                    desktop.method:=2;
            end
            else desktop.color:=clHighLight;
            end;
        finally
            reg.Free;
        end;

        loadPictureDir;

        csBackground.color:=Desktop.color;
        cbPosition.itemindex:=Desktop.method;
        pnColor.color:=csBackground.color;
        if desktop.desktopimage='' then begin
            imgBack.Visible:=false;
            lbPosition.enabled:=false;
            cbPosition.enabled:=false;
        end
        else begin
            loadimage(extractfilename(desktop.desktopimage));
        end;
    finally
        selectenabled:=true;
    end;
end;

procedure TDisplayPropertiesDlg.loadPictureDir;
var
    list:TStringList;
    sr: TSearchRec;
    dir:string;
    i:longint;
    ext: string;
    li:TListItem;
    img: string;
    ts: TListItem;
begin
    //Load a list of images located on the wallpapers dir
    ts:=nil;
    img:=extractfilename(desktop.desktopimage);
    list:=TStringList.create;
    try
        dir:=getSystemInfo(XP_WALLPAPERS_DIR)+'/';
        if findfirst(dir+'*.jpg',faAnyFile,sr)=0 then begin
            repeat
                   list.add(sr.name);
            until findnext(sr)<>0;
            findclose(sr);
        end;
        if findfirst(dir+'*.png',faAnyFile,sr)=0 then begin
            repeat
                   list.add(sr.name);
            until findnext(sr)<>0;
            findclose(sr);
        end;
        if findfirst(dir+'*.bmp',faAnyFile,sr)=0 then begin
            repeat
                   list.add(sr.name);
            until findnext(sr)<>0;
            findclose(sr);
        end;
        list.sorted:=true;
        list.sorted:=false;
        list.Insert(0,sNone);
        lvPictures.Items.BeginUpdate;
        try
            for i:=0 to list.count-1 do begin
                ext:=ansilowercase(extractfileext(list[i]));
                li:=lvPictures.Items.Add;
                if extractfilename(list[i])=img then ts:=li;
                li.Caption:=changefileext(extractfilename(list[i]),'');
                if (ext='.png') then begin
                    li.ImageIndex:=3;
                end
                else
                if (ext='.jpg') then begin
                    li.ImageIndex:=2;
                end
                else
                if ext='.bmp' then begin
                    li.ImageIndex:=1;
                end
                else begin
                    li.ImageIndex:=0;
                    ts:=li;
                end;
            end;
        finally
            lvPictures.items.endupdate;
        end;
    finally
        lvPictures.Selected:=ts;
        lvPictures.Selected.MakeVisible;
        list.free;
    end;
end;

procedure TDisplayPropertiesDlg.loadImage(const filename:string);
var
    dir:string;
    b:TBitmap;
    im:TPicture;
    ax,ay:integer;
begin

    dir:=getSystemInfo(XP_WALLPAPERS_DIR)+'/';
    im:=TPicture.create;
    b:=TBitmap.create;
    try
        im.loadfromfile(dir+filename);
        b.Canvas.brush.color:=csBackground.color;
        b.Width:=screen.width;
        b.height:=screen.Height;
        case cbPosition.ItemIndex of
            0: begin
                ax:=(b.width-im.Graphic.Width) div 2;
                ay:=(b.height-im.Graphic.height) div 2;
                b.Canvas.draw(ax,ay,im.graphic);
            end;
            1: begin
                ax:=0;
                while (ax<b.width) do begin
                    ay:=0;
                    while (ay<b.height) do begin
                        b.Canvas.draw(ax,ay,im.graphic);
                        inc(ay,im.graphic.height);
                    end;
                    inc(ax,im.graphic.width);
                end;
            end;
            2: begin
                b.canvas.StretchDraw(rect(0,0,b.width,b.height),im.graphic);
            end;
        end;
        imgBack.picture.assign(b);
        imgBack.Visible:=true;
        lastimage:=dir+filename;
    finally
        b.free;
        im.free;
    end;
end;

procedure TDisplayPropertiesDlg.lvPicturesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
    ext:string;
begin
    if selectenabled then begin
        if item.ImageIndex=0 then begin
            lbPosition.enabled:=false;
            cbPosition.enabled:=false;
            //Rellenar aqui el imgBack con el color de fondo
            if assigned(imgBack.picture.graphic) then imgBack.Picture.Graphic.assign(nil);
            imgBack.Visible:=false;
            lastimage:='none';
        end
        else begin
            lbPosition.enabled:=true;
            cbPosition.enabled:=true;
            if item.ImageIndex=1 then ext:='.bmp'
            else if item.ImageIndex=2 then ext:='.jpg'
            else if item.ImageIndex=3 then ext:='.png';
            loadImage(Item.caption+ext);
       end;
    end;
end;

procedure TDisplayPropertiesDlg.btnOkClick(Sender: TObject);
begin
    storeproperties;
    XPIPC.broadcastMessage(XPDE_DESKTOPCHANGED,0);
    close;
end;

procedure TDisplayPropertiesDlg.lvPicturesCustomDrawItem(
  Sender: TCustomViewControl; Item: TCustomViewItem; Canvas: TCanvas;
  const Rect: TRect; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
var
    bcolor:TColor;
    b: TBitmap;
    r: TRect;
    tx: integer;
    ty: integer;
    cap: string;
    res: TBitmap;
begin
    canvas.font.name:=(sender as TListview).font.name;
    canvas.font.height:=(sender as TListview).font.height;
    canvas.font.size:=(sender as TListview).font.size;
    bcolor:=canvas.brush.color;
    canvas.brush.color:=clBtnHighlight;
    canvas.pen.color:=clBtnHighlight;
    canvas.Rectangle(rect);
    cap:=(item as TListItem).Caption;
    b:=TBitmap.create;
    res:=TBitmap.create;
    try
        b.width:=(sender as TListView).Images.Width;
        b.height:=(sender as TListView).Images.height;
        (sender as TListView).Images.GetBitmap((item as TListItem).imageindex,b);
        b.transparent:=true;
        if cdsSelected in state then begin
            MaskedBitmap(b,res);
            res.transparent:=true;
            Canvas.Draw(rect.left+4,rect.top,res);
        end
        else Canvas.Draw(rect.left+4,rect.top,b);
    finally
        res.free;
        b.free;
    end;

    tx:=rect.left+4+(sender as TListView).Images.Width;
    ty:=rect.top+1;

    r.left:=tx;
    r.top:=ty;
    r.right:=r.left+canvas.TextWidth(cap)+4+2;
    r.bottom:=r.top+canvas.TextHeight(cap)+2;
    canvas.brush.color:=bcolor;
    canvas.pen.color:=bcolor;
    canvas.Rectangle(r);

    canvas.TextOut(tx+2,ty+1,cap);
    if (cdsSelected in state) then begin
        canvas.DrawFocusRect(r);
    end;
    defaultdraw:=false;
end;
procedure TDisplayPropertiesDlg.csBackgroundChange(Sender: TObject);
begin
    pnColor.color:=csBackground.color;
    lvPicturesSelectItem(lvPictures,lvPictures.Selected,true);
end;

procedure TDisplayPropertiesDlg.FormShow(Sender: TObject);
begin
    left:=(screen.width-clientwidth) div 2;
    top:=(screen.height-clientheight) div 2;
end;
procedure TDisplayPropertiesDlg.cbPositionChange(Sender: TObject);
begin
    lvPicturesSelectItem(lvPictures,lvPictures.Selected,true);
end;
procedure TDisplayPropertiesDlg.btnBrowseClick(Sender: TObject);
var
    dir:string;
    destination: string;
    ls: TListItem;
begin
    if odPictures.execute then begin
        dir:=getSystemInfo(XP_WALLPAPERS_DIR)+'/';
        destination:=odPictures.filename;
        if extractfilepath(odPictures.FileName)<>dir then begin
            destination:=dir+extractfilename(odPictures.filename);
            copyfile(odPictures.filename,destination);
        end;
        ls:=addimagetolist(destination);
        if assigned(ls) then begin
            lvPictures.Selected:=ls;
        end;
    end;
end;


function TDisplayPropertiesDlg.addimagetolist(const filename: string):TListitem;
var
    ext:string;
begin
    ext:=ansilowercase(extractfileext(filename));
    result:=lvPictures.Items.Add;
    result.Caption:=changefileext(extractfilename(filename),'');
    if ext='.png' then begin
        result.ImageIndex:=3;
    end
    else
    if ext='.jpg' then begin
        result.ImageIndex:=2;
    end
    else
        if ext='.bmp' then begin
            result.ImageIndex:=1;
        end
        else begin
            result.ImageIndex:=0;
        end;
end;
procedure TDisplayPropertiesDlg.btnCancelClick(Sender: TObject);
begin
    close;
end;

procedure TDisplayPropertiesDlg.storeproperties;
var
    reg: TRegistry;
begin
    //Aqui tengo que guardar toda la información en el registro
        reg:=TRegistry.create;
        try
            if reg.OpenKey('Software/XPde/Desktop/Background',true) then begin
                reg.WriteString('Image',lastimage);
                reg.WriteInteger('Method',cbPosition.itemindex);
            end;
        finally
            reg.Free;
        end;

        reg:=TRegistry.create;
        try
            if reg.OpenKey('Software/XPde/Desktop/Background',true) then begin
                    reg.WriteString('Color',colortostring(csBackground.color));
            end;
        finally
            reg.free;
        end;
end;

procedure TDisplayPropertiesDlg.btnApplyClick(Sender: TObject);
begin
    storeproperties;
    XPIPC.broadcastMessage(XPDE_DESKTOPCHANGED,0);
end;

procedure TDisplayPropertiesDlg.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
    action:=caFree;
    DisplayPropertiesDlg:=nil;
end;


end.
