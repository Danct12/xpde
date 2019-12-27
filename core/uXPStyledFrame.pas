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
unit uXPStyledFrame;

interface

uses
  SysUtils, Types, Classes,
  Variants, QTypes, QGraphics,
  QControls, QForms, XLib,
  QDialogs, QStdCtrls, QExtCtrls,
  Qt, uCommon, uGraphics,
  uWindowManager, uResample;

type
  TStyledFrame = class;

  //A caption button
  TXPCaptionButton=class(TPanel)
  private
    { TODO : Add support for inactive states }
    FMask: TBitmap;
    FOver: TBitmap;
    FNormal: TBitmap;
    FPressed: TBitmap;
    FGlyph: string;
    procedure SetGlyph(const Value: string);
  public
    frame: TStyledFrame;
    procedure SetParent(const Value:TWidgetControl);override;
    procedure mouseenter(AControl:TControl);override;
    procedure mouseleave(AControl:TControl);override;
    procedure MouseDown(button: TMouseButton; Shift: TShiftState; X, Y: integer);override;
    procedure MouseUp(button: TMouseButton; Shift: TShiftState; X, Y: integer);override;
    procedure setup;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property Glyph: string read FGlyph write SetGlyph;
  end;

  //A frame the can be styled
  TStyledFrame = class(TForm)
    topFrame: TPanel;
    bottomFrame: TPanel;
    middleFrame: TPanel;
    leftFrame: TPanel;
    rightFrame: TPanel;
    bottomFrameLeft: TPanel;
    bottomFrameRight: TPanel;
    clientArea: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure topFrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure topFrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure topFrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure rightFrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure rightFrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure rightFrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure leftFrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure bottomFrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure bottomFrameRightMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure bottomFrameLeftMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }

    { TODO : Organize all frame bitmaps in a common place, to save resources }
    //Masks to created rounded effect
    FLeftMask: TBitmap;
    FRightMask: TBitmap;
    FFullMask: TBitmap;

    //Bottom bitmap
    FBottom: TBitmap;
    FBottomLeft: TBitmap;
    FBottomRight: TBitmap;
    FBottomInactive: TBitmap;
    FBottomLeftInactive: TBitmap;
    FBottomRightInactive: TBitmap;

    //Caption bitmap parts
    FCaption: TBitmap;
    FCaptionLeft: TBitmap;
    FCaptionRight: TBitmap;

    //Left bitmap
    FLeft: TBitmap;
    FLeftInactive: TBitmap;

    //Left bitmap
    FRight: TBitmap;
    FRightInactive: TBitmap;

    FCaptionInactive: TBitmap;
    FCaptionLeftInactive: TBitmap;
    FCaptionRightInactive: TBitmap;

    //Caption buttons
    FCloseButton: TXPCaptionButton;
    FMaxButton: TXPCaptionButton;
    FMinButton: TXPCaptionButton;

    //A value for performance purposes
    lastWidth: integer;

    //Moving and resizing variables
    moving: boolean;
    resizing: boolean;
    ox: integer;
    oy: integer;

    //The window caption
    windowtitle: widestring;
  public
    { Public declarations }
    client:TWMClient;

    //All this must be changed
    { TODO : Optimize the way a window icon is retrieved and stored }
    imgIcon: TImage;
    mini_icon_a: TBitmap;
    mini_icon_i: TBitmap;
    mini_icon_s: TBitmap;
    icon_s: TBitmap;

    normalIcon: TBitmap;
    smallIcon: TBitmap;

    //Used to know the next dimensions of the frame in advance
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    //Setup the frame
    procedure setup;

    //Returns the frame sizes (left, top, right, bottom)
    function getFrameBorderSizes:TRect;

    //Updates the window title (check it)
    procedure updateWindowTitle;

    { TODO : Make the window title a property of the frame }
    procedure setTitle(ATitle: widestring);
    function getTitle:widestring;

    { TODO : Make the client a property }
    procedure setClient(AClient:TWMClient);

    //Resizes all the bitmaps to fit the new dimensions
    procedure resizeBitmaps(AWidth,AHeight:integer);

    //Returns the control that is going to be the parent of the client
    function getHostControl:TWidgetControl;

    //Notifications
    procedure updateWindowState;
    procedure updateActiveState;
    procedure updateClientSize;

    procedure updateButtonsPosition(AWidth:integer);

    //Builds the icons for the window, must be optimized
    procedure setupIcons;

    //Event handlers
    procedure OnCloseButton(Sender:TObject);
    procedure OnMaximizeButton(Sender:TObject);
    procedure OnMinimizeButton(Sender:TObject);
  end;

var
  StyledFrame: TStyledFrame;

implementation

{$R *.xfm}

procedure TStyledFrame.FormCreate(Sender: TObject);
var
    dir: string;
begin
    normalIcon:=TBitmap.create;
    smallIcon:=TBitmap.create;    
    
    //All this must change
    imgIcon:=TImage.create(nil);

    mini_icon_a:=TBitmap.create;
    mini_icon_a.width:=16;
    mini_icon_a.height:=16;

    mini_icon_i:=TBitmap.create;
    mini_icon_i.width:=16;
    mini_icon_i.height:=16;

    mini_icon_s:=TBitmap.create;
    mini_icon_s.width:=16;
    mini_icon_s.height:=16;

    icon_s:=TBitmap.create;
    icon_s.width:=32;
    icon_s.height:=32;


    { TODO : Use the constructor instead FormCreate }
    windowtitle:='';

    //Setup masks used to allow non rect frames to be used
    FLeftMask:=TBitmap.create;
    FRightMask:=TBitmap.create;
    FFullMask:=TBitmap.create;
    dir:=getSystemInfo(XP_FRAME_DIR);
    FLeftMask.LoadFromFile(dir+'/frame_mask_left.bmp');
    FRightMask.LoadFromFile(dir+'/frame_mask_right.bmp');
    FFullMask.PixelFormat:=pf1bit;
    FFullMask.assign(FLeftMask);
    FFullMask.Width:=qforms.screen.width;
    FFullMask.height:=qforms.screen.height;

    //Some helper vars
    moving:=false;
    lastwidth:=-1;

    //Creates bitmaps needed for the frame
    FBottom:=TBitmap.create;
    FBottomLeft:=TBitmap.create;
    FBottomRight:=TBitmap.create;
    FBottomInactive:=TBitmap.create;
    FBottomLeftInactive:=TBitmap.create;
    FBottomRightInactive:=TBitmap.create;

    FCaption:=TBitmap.create;
    FCaptionLeft:=TBitmap.create;
    FCaptionRight:=TBitmap.create;

    FCaptionInactive:=TBitmap.create;
    FCaptionLeftInactive:=TBitmap.create;
    FCaptionRightInactive:=TBitmap.create;

    FLeft:=TBitmap.create;
    FLeftInactive:=TBitmap.create;

    FRight:=TBitmap.create;
    FRightInactive:=TBitmap.create;

    //Caption buttons
    FCloseButton:=TXPCaptionButton.create(nil);
    FCloseButton.frame:=self;
    FCloseButton.Glyph:='close';

    FMaxButton:=TXPCaptionButton.create(nil);
    FMaxButton.frame:=self;
    FMaxButton.Glyph:='maximize';

    FMinButton:=TXPCaptionButton.create(nil);
    FMinButton.frame:=self;    
    FMinButton.Glyph:='minimize';

    //Setup the frame    
    setup;
end;

procedure TStyledFrame.setup;
var
    dir: string;
begin
    dir:=getSystemInfo(XP_FRAME_DIR);

    //Frames only yet support tiled left and and right frames
    FLeft.LoadFromFile(dir+'/frameleft_active.png');
    FLeftInactive.LoadFromFile(dir+'/frameleft_inactive.png');    
    leftFrame.Bitmap.assign(FLeft);
    leftFrame.Width:=leftFrame.bitmap.width;

    FRight.LoadFromFile(dir+'/frameright_active.png');
    FRightInactive.LoadFromFile(dir+'/frameright_inactive.png');
    rightFrame.Bitmap.assign(FRight);
    rightFrame.Width:=rightFrame.bitmap.width;


    //Bottom frame is split in three parts, two corners and a central part that is stretched as needed
    FBottom.LoadFromFile(dir+'/framebottom_active.png');
    FBottomInactive.LoadFromFile(dir+'/framebottom_inactive.png');
    bottomFrame.bitmap.assign(FBottom);
    bottomFrame.height:=FBottom.height;

    FBottomLeft.LoadFromFile(dir+'/framebottom_left_active.png');
    FBottomLeftInactive.LoadFromFile(dir+'/framebottom_left_inactive.png');
    bottomFrameLeft.Bitmap.assign(FBottomLeft);
    bottomFrameLeft.Width:=bottomFrameLeft.bitmap.width;

    FBottomRight.LoadFromFile(dir+'/framebottom_right_active.png');
    FBottomRightInactive.LoadFromFile(dir+'/framebottom_right_inactive.png');
    bottomFrameRight.bitmap.assign(FBottomRight);
    bottomFrameRight.Width:=bottomFrameRight.bitmap.width;

    //Caption is also built from three parts
    FCaption.LoadFromFile(dir+'/captionbar_active.png');
    FCaptionInactive.LoadFromFile(dir+'/captionbar_inactive.png');
    topFrame.bitmap.assign(FCaption);
    topFrame.height:=FCaption.height;

    FCaptionLeft.LoadFromFile(dir+'/captionbar_left_active.png');
    FCaptionLeftInactive.LoadFromFile(dir+'/captionbar_left_inactive.png');

    FCaptionRight.LoadFromFile(dir+'/captionbar_right_active.png');
    FCaptionRightInactive.LoadFromFile(dir+'/captionbar_right_inactive.png');

    //Set buttons positions
    //Don't use anchors here because are a bit slower
    updateButtonsPosition(Width);
    FCloseButton.parent:=topFrame;
    FCloseButton.OnClick:=OnCloseButton;
    
    FMaxButton.parent:=topFrame;
    FMaxButton.OnClick:=OnMaximizeButton;

    FMinButton.parent:=topFrame;
    FMinButton.OnClick:=OnMinimizeButton;
end;

procedure TStyledFrame.FormDestroy(Sender: TObject);
begin
    normalIcon.free;
    smallIcon.free;    
    
    //Destroy all objects not needed
    imgIcon.free;
    mini_icon_a.free;
    mini_icon_i.free;
    mini_icon_s.free;
    icon_s.free;

    FLeftMask.free;
    FRightMask.free;
    FCloseButton.free;
    FMaxButton.free;
    FMinButton.free;

    FBottom.free;
    FBottomLeft.free;
    FBottomRight.free;
    FBottomLeftInactive.free;
    FBottomRightInactive.free;
    FCaption.free;
    FCaptionLeft.free;
    FCaptionRight.free;
    FLeft.free;
    FRight.free;

    FBottomInactive.free;
    FCaptionInactive.free;
    FCaptionLeftInactive.free;
    FCaptionRightInactive.free;
    FLeftInactive.free;
    FRightInactive.free;
end;

procedure TStyledFrame.resizeBitmaps(AWidth,AHeight:integer);
var
    w: integer;
    t: integer;
    temp: TBitmap;
    atitle: string;
    k: integer;
const
    caption_x=28;
    caption_y=7;
    style=[fsBold];
    fsize=11;

    ix=8;
    iy=7;
begin
    //Resize bitmaps to fit the new frame dimensions
    if (assigned(bottomFrameLeft)) then begin
        temp:=TBitmap.create;
        try
            if (AWidth<Constraints.MinWidth) then AWidth:=Constraints.MinWidth;
             
            w:=AWidth-bottomFrameLeft.width-bottomFrameRight.width;
            t:=AWidth-FCaptionLeft.width-FCaptionRight.width;
            if (lastWidth<>w) then begin
                //This is the bottom frame
                bottomFrame.Bitmap.width:=AWidth;
                if client.isactive then begin
                    leftFrame.bitmap.assign(FLeft);
                    rightFrame.bitmap.assign(FRight);
                    bottomFrameLeft.Bitmap.Assign(FBottomLeft);
                    bottomFrameRight.Bitmap.Assign(FBottomRight);
                    bottomFrame.Bitmap.Canvas.StretchDraw(rect(bottomFrameLeft.width,0,bottomFrameLeft.width+w,bottomFrame.Height),FBottom);
                end
                else begin
                    leftFrame.bitmap.assign(FLeftInactive);
                    rightFrame.bitmap.assign(FRightInactive);
                    bottomFrameLeft.Bitmap.Assign(FBottomLeftInactive);
                    bottomFrameRight.Bitmap.Assign(FBottomRightInactive);
                    bottomFrame.Bitmap.Canvas.StretchDraw(rect(bottomFrameLeft.width,0,bottomFrameLeft.width+w,bottomFrame.Height),FBottomInactive);
                end;

                //This is the top frame
                temp.assign(topFrame.bitmap);
                temp.width:=AWidth;
                if (client.isactive) then begin
                    temp.canvas.draw(0,0,FCaptionLeft);
                    temp.Canvas.StretchDraw(rect(FCaptionLeft.width,0,FCaptionLeft.width+t,topFrame.Height),FCaption);
                    temp.canvas.draw(AWidth-FCaptionRight.width,0,FCaptionRight);
                end
                else begin
                    temp.canvas.draw(0,0,FCaptionLeftInactive);
                    temp.Canvas.StretchDraw(rect(FCaptionLeft.width,0,FCaptionLeft.width+t,topFrame.Height),FCaptionInactive);
                    temp.canvas.draw(AWidth-FCaptionRight.width,0,FCaptionRightInactive);
                end;

                temp.Canvas.draw(ix,iy, smallIcon);

                temp.canvas.font.Size:=fsize;
                temp.canvas.font.Style:=style;
                atitle:=windowtitle;

                k:=0;
                while (canvas.TextWidth(atitle)>FMinButton.Left-caption_x-32) do begin
                    atitle:=copy(windowtitle,1,length(windowtitle)-k)+'...';
                    if (atitle='...') then break;
                    inc(k);
                end;

                temp.canvas.font.color:=clGray;
                temp.Canvas.TextOut(caption_x+1,caption_y+1,atitle);
                if (client.isactive) then temp.canvas.font.color:=clWhite
                else temp.canvas.font.color:=rgbtocolor(200,200,200);
                temp.Canvas.TextOut(caption_x,caption_y,atitle);

                topFrame.Bitmap.assign(temp);
                topFrame.refresh;
                lastwidth:=w;

                //This is the mask
                FFullMask.Canvas.draw(0,0,FLeftMask);
                FFullMask.Canvas.fillrect(Rect(FLeftMask.width,0, AWidth-FRightMask.width,FLeftMask.height));
                FFullMask.Canvas.draw(AWidth-FRightMask.width,0,FRightMask);
                QWidget_setMask(self.handle,QBitmapH(FFullMask.handle));

            end;
        finally
            temp.free;
        end;
    end;
end;

procedure TStyledFrame.ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  //The very first, resize bitmaps
  resizeBitmaps(AWidth,AHeight);
  inherited;
  //Then, reposition buttons
  updateButtonsPosition(AWidth);
end;

procedure TStyledFrame.topFrameMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    //Initiates moving
    { TODO : Dblclick must maximize window }
    if (not client.isactive) then client.activate;    
    moving:=true;
    ox:=x;
    oy:=y;
end;

procedure TStyledFrame.topFrameMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
    //Moves window
    if moving then begin
        //Why more? ;-)
        XSync(xpwindowmanager.display,1);
        QWidget_move(self.handle,left+(x-ox),top+(y-oy));
        XSync(xpwindowmanager.display,0);
   end
end;

procedure TStyledFrame.topFrameMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    //Stops movement
    moving:=false;

    //Sends new coordinates to the client
    client.sendsyntheticConfigureNotify;
end;

{ TXPCaptionButton }

constructor TXPCaptionButton.Create(AOwner: TComponent);
begin
  inherited;
  frame:=nil;
  FMask:=TBitmap.create;
  FNormal:=TBitmap.create;
  FOver:=TBitmap.create;
  FPressed:=TBitmap.create;
  setup;
end;

destructor TXPCaptionButton.Destroy;
begin
  FMask.free;
  FNormal.free;
  FOver.free;
  FPressed.free;
  inherited;
end;

procedure TXPCaptionButton.MouseDown(button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited;
  if (assigned(frame)) then begin
    if assigned(frame.client) then frame.client.activate;    
  end;
  bitmap.Assign(FPressed);
end;

procedure TXPCaptionButton.mouseenter(AControl: TControl);
begin
  inherited;
  bitmap.Assign(FOver); 
end;

procedure TXPCaptionButton.mouseleave(AControl: TControl);
begin
  inherited;
  bitmap.Assign(FNormal);
end;

procedure TXPCaptionButton.MouseUp(button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited;
  if ptinrect(clientrect,point(x,y)) then bitmap.assign(FOver)
  else begin
    bitmap.Assign(FNormal);
  end;
end;

procedure TXPCaptionButton.SetGlyph(const Value: string);
begin
    if (value<>FGlyph) then begin
        FGlyph := Value;
        setup;
    end;
end;

procedure TXPCaptionButton.SetParent(const Value: TWidgetControl);
var
    dir: string;
begin
    inherited;
    if (Parent<>nil) then begin
        dir:=getSystemInfo(XP_FRAME_DIR);
        FMask.LoadFromFile(dir+'/captionbutton_mask.bmp');
        QWidget_setMask(self.handle, QBitmapH(FMask.handle));
    end;
end;

procedure TXPCaptionButton.setup;
var
    dir: string;
    glyph: TBitmap;
    gx,gy: integer;
begin
    BevelOuter:=bvNone;
    dir:=getSystemInfo(XP_FRAME_DIR);
    FNormal.LoadFromFile(dir+'/captionbutton_normal.png');
    FOver.LoadFromFile(dir+'/captionbutton_over.png');
    FPressed.LoadFromFile(dir+'/captionbutton_press.png');
    FNormal.transparent:=true;


    { TODO : Button sizes must be read from the themes or from bitmap dimensions }
    width:=19;
    height:=18;

    if (FGlyph<>'') then begin
        glyph:=TBitmap.create;
        try
            glyph.LoadFromFile(dir+'/'+FGlyph+'_active_normal.png');
            glyph.Transparent:=true;
            gx:=(FNormal.width - glyph.width) div 2;
            gy:=((FNormal.height - glyph.height) div 2)+1;
            FNormal.Canvas.Draw(gx,gy,glyph);

            glyph.LoadFromFile(dir+'/'+FGlyph+'_active_over.png');
            glyph.Transparent:=true;
            FOver.Canvas.Draw(gx,gy,glyph);

            glyph.LoadFromFile(dir+'/'+FGlyph+'_active_press.png');
            glyph.Transparent:=true;
            FPressed.Canvas.Draw(gx,gy,glyph);
        finally
            glyph.free;
        end;
    end;
    bitmap.assign(FNormal);
end;

procedure TStyledFrame.rightFrameMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
    size: TRect;
begin
    //Resizes horizontally
    if resizing then begin
        size:=boundsrect;
        size.Right:=size.right+(x-ox);
        BoundsRect:=size;
        updateclientsize;
    end;
end;

procedure TStyledFrame.rightFrameMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if (not client.isactive) then client.activate;
    ox:=x;
    oy:=y;
    resizing:=true;
end;

procedure TStyledFrame.rightFrameMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    resizing:=false;
end;

procedure TStyledFrame.leftFrameMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
    size: TRect;
begin
    //Resizes horizontally
    if resizing then begin
        size:=boundsrect;
        size.left:=size.left+(x-ox);
        BoundsRect:=size;
        updateclientsize;
    end;
end;

procedure TStyledFrame.bottomFrameMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
    size: TRect;
begin
    //Resizes vertically
    if resizing then begin
        size:=boundsrect;
        size.bottom:=size.bottom+(y-oy);
        BoundsRect:=size;
        updateclientsize;
    end;
end;

procedure TStyledFrame.bottomFrameRightMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
    size: TRect;
begin
    //Resizes NWSE
    if resizing then begin
        size:=boundsrect;
        size.right:=size.right+(x-ox);
        size.bottom:=size.bottom+(y-oy);
        BoundsRect:=size;
        updateclientsize;
    end;
end;

procedure TStyledFrame.bottomFrameLeftMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
    size: TRect;
begin
    //Resizes NESW
    if resizing then begin
        size:=boundsrect;
        size.left:=size.left+(x-ox);
        size.bottom:=size.bottom+(y-oy);
        BoundsRect:=size;
        updateclientsize;
    end;
end;

procedure TStyledFrame.setClient(AClient: TWMClient);
begin
    client:=AClient;
end;

procedure TStyledFrame.setTitle(ATitle: widestring);
begin
    windowtitle:=ATitle;
    updatewindowtitle;
end;

procedure TStyledFrame.updateWindowTitle;
begin
    { TODO : Update window title }
    lastwidth:=-1;    
    resizeBitmaps(Width,height);
end;

function TStyledFrame.getFrameBorderSizes: TRect;
begin
    result.left:=leftFrame.width;
    result.top:=topFrame.height;
    result.right:=rightFrame.width;
    result.bottom:=bottomFrame.height;
end;

procedure TStyledFrame.setupIcons;
var
    s: TBitmap;
    m: TBitmap;
    x,y: integer;
    source: TBitmap;

    image: QImageH;
    aimage: QImageH;
begin
    s:=TBitmap.create;
    try
        s.assign(imgIcon.picture.bitmap);

        image := QImage_create;
        QPixmap_convertToImage(s.handle,image);

        aimage := QImage_create;
        QImage_smoothScale(image,aimage,32,32);

        QPixmap_convertFromImage(s.handle,aimage,0);

        normalIcon.canvas.brush.color:=rgbtocolor(255,0,255);
        normalIcon.Width:=32;
        normalIcon.height:=32;
        normalIcon.Canvas.Draw(0,0,s);
        normalIcon.transparent:=true;
    finally
        s.free;
    end;

    s:=TBitmap.create;
    try
        s.assign(imgIcon.picture.bitmap);

        image := QImage_create;
        QPixmap_convertToImage(s.handle,image);

        aimage := QImage_create;
        QImage_smoothScale(image,aimage,16,16);

        QPixmap_convertFromImage(s.handle,aimage,0);

        smallIcon.canvas.brush.color:=rgbtocolor(255,0,255);
        smallIcon.Width:=16;
        smallIcon.height:=16;
        smallIcon.Canvas.Draw(0,0,s);
        smallIcon.transparent:=true;
    finally
        s.free;
    end;


    (*
    { TODO : This must be completely changed }
    s:=TBitmap.create;
    try
        s.Canvas.Brush.Color:=$6b2408;
        s.canvas.pen.color:=$6b2408;
        s.width:=imgIcon.picture.bitmap.width;
        s.height:=imgIcon.picture.bitmap.height;
        s.Canvas.Draw(0,0,imgIcon.picture.bitmap);
        strecth(s,mini_icon_a,resampleFilters[1].filter,resamplefilters[1].width);
    finally
        s.free;
    end;

    s:=TBitmap.create;
    try
        s.Canvas.Brush.Color:=clGray;
        s.canvas.pen.color:=clGray;
        s.width:=imgIcon.picture.bitmap.width;
        s.height:=imgIcon.picture.bitmap.height;
        s.Canvas.Draw(0,0,imgIcon.picture.bitmap);
        strecth(s,mini_icon_i,resampleFilters[1].filter,resamplefilters[1].width);
    finally
        s.free;
    end;

    m:=TBitmap.create;
    try

        m.width:=16;
        m.height:=16;
        m.Canvas.Brush.Color:=clFuchsia;
        m.canvas.pen.color:=clFuchsia;
        m.Canvas.StretchDraw(rect(0,0,16,16),imgIcon.Picture.Bitmap);

        strecth(imgIcon.picture.bitmap,mini_icon_s,resampleFilters[1].filter,resamplefilters[1].width);

        for y:=0 to 15 do begin
            for x:=0 to 15 do begin
                if m.Canvas.Pixels[x,y]=clFuchsia then begin
                    mini_icon_s.Canvas.Pixels[x,y]:=clFuchsia;
                end;
            end;
        end;
    finally
        m.free;
    end;

    m:=TBitmap.create;
    try

        m.width:=32;
        m.height:=32;
        m.Canvas.Brush.Color:=clFuchsia;
        m.canvas.pen.color:=clFuchsia;
        m.Canvas.StretchDraw(rect(0,0,32,32),imgIcon.Picture.Bitmap);

        strecth(imgIcon.picture.bitmap,icon_s,resampleFilters[1].filter,resamplefilters[1].width);

        for y:=0 to 31 do begin
            for x:=0 to 31 do begin
                if m.Canvas.Pixels[x,y]=clFuchsia then begin
                    icon_s.Canvas.Pixels[x,y]:=clFuchsia;
                end;
            end;
        end;
    finally
        m.free;
    end;
    *)
end;

function TStyledFrame.getTitle: widestring;
begin
    result:=windowtitle;
end;

procedure TStyledFrame.updateWindowState;
begin
    if client.WindowState=wsMaximized then begin
        FMaxButton.Glyph:='restore';
    end
    else begin
        FMaxButton.Glyph:='maximize';    
    end;
end;

function TStyledFrame.getHostControl: TWidgetControl;
begin
    result:=clientArea;
end;

procedure TStyledFrame.updateActiveState;
begin
    if assigned(client) then begin
        lastWidth:=-1;
        resizeBitmaps(width,height);
    end;
end;

procedure TStyledFrame.updateclientsize;
begin
    if assigned(client) then begin
        client.beginresize;
        try
            XSync(xpwindowmanager.display,1);
            XResizeWindow(XPWindowManager.Display,client.getwindow,clientArea.width,clientArea.height);
            XSync(xpwindowmanager.display,0);
        finally
            client.endresize;
        end;
    end;
end;

procedure TStyledFrame.updateButtonsPosition(AWidth:integer);
begin
  if assigned(FCloseButton) then begin
    if (AWidth<Constraints.MinWidth) then AWidth:=constraints.minwidth;
    
    FCloseButton.top:=((topFrame.Height - FCloseButton.Height) div 2)+2;
    FCloseButton.Left:=AWidth-FCloseButton.width-7;

    FMaxButton.Top:=FCloseButton.Top;
    FMaxButton.Left:=AWidth-FMaxButton.width-7-FCloseButton.width-4;

    FMinButton.Top:=FCloseButton.Top;
    FMinButton.Left:=AWidth-FMinButton.width-7-FCloseButton.width-4-FMaxButton.width-4;
  end;
end;

procedure TStyledFrame.OnCloseButton(Sender: TObject);
begin
    if assigned(client) then client.close;
end;

procedure TStyledFrame.OnMaximizeButton(Sender: TObject);
begin
    if assigned(client) then begin
        if client.windowstate<>wsMaximized then begin
            client.maximize;
        end
        else begin
            client.restore;
        end;
    end;
end;

procedure TStyledFrame.OnMinimizeButton(Sender: TObject);
begin
    if assigned(client) then begin
        if client.windowstate<>wsMinimized then begin
            client.minimize;
        end
        else begin
            client.restore;
        end;
    end;
end;

initialization
    XPWindowManager.Frame:=TStyledFrame;

end.
