{******************************************************************************}
{* This software is provided 'as-is', without any express or                  *}
{* implied warranty. In no event will the author be held liable               *}
{* for any damages arising from the use of this software.                     *}
{*                                                                            *}
{* Permission is granted to anyone to use this software for any               *}
{* purpose, including commercial applications, and to alter it                *}
{* and redistribute it freely, subject to the following                       *}
{* restrictions:                                                              *}
{*                                                                            *}
{*   1. The origin of this software must not be misrepresented,               *}
{*      you must not claim that you wrote the original software.              *}
{*      If you use this software in a product, an acknowledgment              *}
{*      in the product documentation would be appreciated but is              *}
{*      not required.                                                         *}
{*                                                                            *}
{*   2. Altered source versions must be plainly marked as such, and           *}
{*      must not be misrepresented as being the original software.            *}
{*                                                                            *}
{*   3. This notice may not be removed or altered from any source             *}
{*      distribution.                                                         *}
{*                                                                            *}
{*                                                                            *}
{* The initial developer is Andreas Hausladen (Andreas.Hausladen@gmx.de)      *}
{*                                                                            *}
{******************************************************************************}
{.$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

{$IFDEF LINUX}
 {.$DEFINE USE_WHITE_RADIOMASK}
{$ENDIF LINUX}

unit QThemed;
interface
uses
  {$IFDEF MSWINDOWS}
  Windows,
  QThemeSrv,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Libc, Xlib,
  QThemeSrvLinux,
  {$ENDIF LINUX}
  Qt, QTypes, SysUtils, Classes, Types, Math, QGraphics, QControls,
  QForms, QStdCtrls, QExtCtrls, QComCtrls, QButtons, QMenus, QImgList, QGrids,
  QStyle;

type
  TRedirectCode = packed record
    Jump: Byte;
    Offset: Integer;
    RealProc: Pointer;
  end;

  TEventFilter = function(Sender: QObjectH; Event: QEventH): Boolean of object;
  QClxStyle_drawToolButton_Event = procedure (p: QPainterH; x: Integer;
    y: Integer; w: Integer; h: Integer; g: QColorGroupH; sunken: Boolean;
    fill: QBrushH) of object cdecl;
  QClxStyle_drawToolButton2_Event = procedure (btn: QToolButtonH;
    p: QPainterH) of object cdecl;

  TThemedStyleNotify = class(TComponent)
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

  TThemedStyle = class(TApplicationStyle)
  private
    FOldAppIdle: TNotifyEvent;
    FCodeQFrame_drawFrame,
    FCodeDrawButtonFace, FCodeDrawEdge, FCodeDrawShadePanel,
    FCodeCustomTabControlDoHotTrack,
    FCodeDrawTab,
    FCodeProgressBarPaint,
    FCodeWidgetControlPainting,
    FCodeToolButtonPaint, {FCodeToolBarPaint,}
    FCodeSpeedButtonPaint,
    FCodeControlMouseMove, FCodeListViewHeaderEventFilters: TRedirectCode;
    FOrgListViewHeaderEventFilter: Pointer;

    FDoHotTrack: Pointer; // address of TCustomTabControl.DoHotTrack

    FMenusEnabled: Boolean;

    FControlInfo: TStrings;
    FNotifyObject: TThemedStyleNotify;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure InternalDrawTab(Index: Integer);
  protected
    procedure DrawPushButtonHook(btn: QPushButtonH; p: QPainterH; var Stage: Integer); cdecl;
    procedure DrawPopupMenuItemHook(p: QPainterH;
      checkable: Boolean; maxpmw, tab: Integer; mi: QMenuItemH; itemID: Integer;
      act, enabled: Boolean; x, y, w, h: Integer; var Stage: Integer); cdecl;
    {$IFDEF LINUX}
    procedure DoDrawToolButtonHook(p: QPainterH; x, y, w, h: Integer; g: QColorGroupH;
      sunken: Boolean; fill: QBrushH); cdecl;
    procedure DoDrawToolButton2Hook(btn: QToolButtonH; p: QPainterH); cdecl;
    {$ENDIF LINUX}
    procedure DoDrawMenuItemObject(Item: QMenuItemH; Canvas: TCanvas;
      Highlighted, Enabled: Boolean; const Rect: TRect; Checkable: Boolean;
      CheckMaxWidth, LabelWidth: Integer; var Stage: Integer);
  protected
    {$IFDEF LINUX}
    procedure DrawQToolButton(Canvas: TCanvas; R: TRect; Down, Focused: Boolean); virtual;
    {$ENDIF LINUX}
    procedure EvAfterDrawButton(Sender, Source: TObject;
      Canvas: TCanvas); virtual;
    procedure EvBeforeDrawButton(Sender, Source: TObject;
      Canvas: TCanvas; var DefaultDraw: Boolean); virtual;
    procedure DoAfterDrawButtonWidget(btn: QPushButtonH; Canvas: TCanvas);
    function DoBeforeDrawButtonWidget(btn: QPushButtonH; Canvas: TCanvas): Boolean;

    procedure EvDrawTrackBar(Sender: TObject; Canvas: TCanvas; const Rect: TRect;
      Horizontal, TickAbove, TickBelow: Boolean; var DefaultDraw: Boolean); virtual;
    procedure EvDrawTrackBarMask(Sender: TObject; Canvas: TCanvas; const Rect: TRect;
      Horizontal, TickAbove, TickBelow: Boolean; var DefaultDraw: Boolean); virtual;
    procedure EvDrawTrackBarGroove(Sender: TObject; Canvas: TCanvas;
      const Rect: TRect; Horizontal: Boolean; var DefaultDraw: Boolean); virtual;

    procedure EvDrawComboButton(Sender: TObject; Canvas: TCanvas; const Rect: TRect;
      Sunken, ReadOnly, Enabled: Boolean; var DefaultDraw: Boolean); virtual;
    procedure EvDrawCheck(Sender: TObject; Canvas: TCanvas; const Rect: TRect;
      Checked, Grayed, Down, Enabled: Boolean; var DefaultDraw: Boolean); virtual;
    procedure EvDrawRadio(Sender: TObject; Canvas: TCanvas; const Rect: TRect;
      Checked, Down, Enabled: Boolean; var DefaultDraw: Boolean); virtual;
    procedure EvDrawRadioMask(Sender: TObject; Canvas: TCanvas; const Rect: TRect;
      Checked: Boolean);
    procedure EvDrawFrame(Sender: TObject; Canvas: TCanvas; const Rect: TRect;
      Sunken: Boolean; LineWidth: Integer; var DefaultDraw: Boolean); virtual;
    procedure EvDrawScrollBar(Sender: TObject; ScrollBar: QScrollBarH;
      Canvas: TCanvas; const Rect: TRect; SliderStart, SliderLength,
      ButtonSize: Integer; Controls: TScrollBarControls;
      DownControl: TScrollBarControl; var DefaultDraw: Boolean); virtual;
    procedure EvDrawHeaderSection(Sender: TObject; Canvas: TCanvas;
      const Rect: TRect; Down: Boolean; var DefaultDraw: Boolean); virtual;

    procedure EvDrawMenuFrame(Sender: TObject; Canvas: TCanvas; const R: TRect;
      LineWidth: Integer; var DefaultDraw: Boolean);
    procedure EvMenuItemHeight(Sender, Source: TObject; Checkable: Boolean;
      FontMetrics: QFontMetricsH; var Height: Integer);
  protected
    procedure HookedCustomTabControlPaint; virtual;
    procedure HookedDoHotTrack(Index: Integer); virtual;
    procedure HookedTabSheetPaint; virtual;
    procedure HookedProgressBarPaint; virtual;
    procedure HookedWidgetControlPainting(Sender: QObjectH; EventRegion: QRegionH); virtual;
    procedure HookedToolButtonPaint; virtual;
//    procedure HookedToolBarPaint; virtual;
    procedure HookedSpeedButtonPaint; virtual;
    procedure HookedControlMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    function HookedListViewHeaderEventFilter(Sender: QObjectH;
      Event: QEventH): Boolean;
  protected
    procedure AppIdle(Sender: TObject); virtual;
    procedure MouseEnter(Control: TControl); virtual;
    procedure MouseLeave(Control: TControl); virtual;
    procedure DrawGroupBox(Control: TCustomGroupBox; Canvas: TCanvas); virtual;

    procedure SetDefaultStyle(const Value: TDefaultStyle); override;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    property MenusEnabled: Boolean read FMenusEnabled write FMenusEnabled;
    property Active: Boolean read GetActive write SetActive;
  end;

function IsMouseOver(Control: TControl): Boolean; overload;
function IsMouseOver(Widget: QWidgetH; ARect: PRect): Boolean; overload;
procedure RepaintControl(Control: TControl);
procedure PaintWidget(Widget: QWidgetH);
procedure PaintControl(Control: TControl);
procedure QExcludeClipRect(DC: QPainterH; x0, y0, x1, y1: Integer);
function GetWindowFromPainter(Painter: QPainterH): Cardinal; // needs Canvas.Start/call/Stop
function GetWidgetFromPainter(Painter: QPainterH): QWidgetH; // needs Canvas.Start/call/Stop


var
  ThemedStyle: TThemedStyle;

implementation

uses uCommon;

{$IFDEF MSWINDOWS}
{$R winxp.res}
{$ENDIF MSWINDOWS}

type
  TOpenControl = class(TControl);
  TOpenWidgetControl = class(TWidgetControl);
  TOpenCustomTabControl = class(TCustomTabControl);
  TOpenTab = class(TTab);
  TOpenCustomControl = class(TCustomControl);
  TOpenGraphicControl = class(TGraphicControl);
  TOpenProgressBar = class(TProgressBar);
  TOpenCustomGroupBox = class(TCustomGroupBox);
  TOpenFrameControl = class(TFrameControl);
  TOpenApplication = class(TApplication);
  TOpenToolButton = class(TToolButton);
  TOpenToolBar = class(TToolBar);
  TOpenSpeedButton = class(TSpeedButton);
  TOpenCustomHeaderControl = class(TCustomHeaderControl);
  TOpenCustomViewControl = class(TCustomViewControl);

  TPrivateApplication = class(TComponent) // Delphi 6, Delphi 7, Kylix 3
  protected
    FHelpSystem: {IHelpSystem}IInterface;
    FArgv: PPChar;
    FTerminated: Boolean;
    FActive: Boolean;
    FShowMainForm: Boolean;
    FQtAccels: Boolean;
    FHintShortCuts: Boolean;
    FEffects: TAppEffects;
    FTimerMode: TTimerMode;
    FHintActive: Boolean;
    FShowHint: Boolean;
    FMinimized: Boolean;
    FMainFormSet: Boolean;
    FKeyState: TShiftState;
    FHandle: QApplicationH;
    FHooks: QObject_hookH;
    FOnException: TExceptionEvent;
    FMainForm: TCustomForm;
    FAppWidget: QWidgetH;
    FTitle: WideString;
    FHint: WideString;
    FHintColor: TColor;
    FHintControl: TControl;
    FHintCursorRect: TRect;
    FHintHidePause: Integer;
    FHintPause: Integer;
    FHintShortPause: Integer;
    FHintTimer: TComponent;
    FHintWindow: THintWindow;
    FIdleTimer: TComponent;
    FMouseControl: TControl; // we need this
  end;

  TPrivateCustomTabControl = class(TCustomControl) // Delphi 6 (patched), Delphi 7, Kylix 3
  protected
    FErase: Boolean;
    FHotTrack: Boolean;
    FMultiLine: Boolean;
    FMultiSelect: Boolean;
    FOwnerDraw: Boolean;
    FRaggedRight: Boolean;
    FShowFrame: Boolean;
    FUpdating: Boolean;
    FStyle: TTabStyle;
    FBitmap: TBitmap;
    FButtons: array [TTabButtons] of TSpeedButton;
    FDblBuffer: TBitmap;
    FFirstVisibleTab: Integer;
    FHotImages: TCustomImageList;
    FHotTrackColor: TColor;
    FImageBorder: Integer;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FLastVisibleTab: Integer;
    FLayoutCount: Integer;
    FMouseOver: Integer;
    FRowCount: Integer;
    FTabIndex: Integer;
    FTabs: TTabs;
    FTabSize: TSmallPoint;
    FTracking: Integer;
  end;

  TPrivateCustomControl = class(TWidgetControl)
  protected
    FCanvas: TCanvas;
  end;

  TPrivateGraphicControl = class(TControl)
  protected
    FCanvas: TCanvas;
  end;

  TPrivateListViewHeader = class(TCustomHeaderControl)
  protected
    FHidden: Boolean;
  end;


function IsMouseOver(Control: TControl): Boolean;
var
  Pt: TPoint;
begin
  if Control <> nil then
  begin
    Pt := Control.ScreenToClient(Mouse.CursorPos);
    Result := PtInRect(Control.ClientRect, Pt);
    if Result then
    begin
      if Control is TWidgetControl then
        Result := FindControl(Mouse.CursorPos) = Control
      else
        Result := FindControl(Mouse.CursorPos) = Control.Parent;
    end;
  end
  else
    Result := False;
end;

function IsMouseOver(Widget: QWidgetH; ARect: PRect): Boolean;
var
  Pt: TPoint;
  R: TRect;
begin
  if Widget <> nil then
  begin
    Pt := Mouse.CursorPos;
    QWidget_mapFromGlobal(Widget, @Pt, @Pt);
    if ARect = nil then
    begin
      QWidget_geometry(Widget, @R);
      OffsetRect(R, -R.Left, -R.Top);
    end
    else
      R := ARect^;
    Result := PtInRect(R, Pt);
  end
  else
    Result := False;
end;

procedure RepaintControl(Control: TControl);
begin
  if Control is TWidgetControl then
    QWidget_repaint(TWidgetControl(Control).Handle)
  else
    Control.Repaint;
end;

procedure PaintWidget(Widget: QWidgetH);
var
  R: TRect;
  EventRegion: QRegionH;
  ForcedPaintEvent: QPaintEventH;
begin
  QWidget_geometry(Widget, @R);
  OffsetRect(R, -R.Left, -R.Top);

  EventRegion := QRegion_create(@R, QRegionRegionType_Rectangle);
  ForcedPaintEvent := QPaintEvent_create(EventRegion, False);
  try
    QObject_event(Widget, ForcedPaintEvent);
  finally
    QPaintEvent_destroy(ForcedPaintEvent);
    QRegion_destroy(EventRegion);
  end;
end;

procedure PaintControl(Control: TControl);
var
  R: TRect;
  ControlRegion: QRegionH;
  WidgetControl: TWidgetControl;
begin
  R := Control.BoundsRect;
  if Control is TWidgetControl then
  begin
    WidgetControl := TWidgetControl(Control);
    OffsetRect(R, -R.Left, -R.Top);
  end
  else
    WidgetControl := Control.Parent;

  ControlRegion := QRegion_create(@R, QRegionRegionType_Rectangle);
  try
    TOpenWidgetControl(WidgetControl).Painting(WidgetControl.Handle, ControlRegion);
  finally
    QRegion_destroy(ControlRegion);
  end;
end;

procedure QExcludeClipRect(DC: QPainterH; x0, y0, x1, y1: Integer);
var
  ExcludeRgn, Rgn: QRegionH;
  Matrix: QWMatrixH;
begin
  if QPainter_hasWorldXForm(DC) then
  begin
    Matrix := QPainter_worldMatrix(DC);
    QWMatrix_map(Matrix, x0, y0, @x0, @y0);
    QWMatrix_map(Matrix, x1, y1, @x1, @y1);
  end;

  ExcludeRgn := QRegion_create(x0, y0, x1 - x0, y1 - y0, QRegionRegionType_Rectangle);
  Rgn := QPainter_clipRegion(DC);
  QRegion_subtract(Rgn, Rgn, ExcludeRgn);
  QPainter_setClipRegion(DC, Rgn);
  QPainter_setClipping(DC, True);
  QRegion_destroy(ExcludeRgn);
end;

procedure QIntersectClipRect(DC: QPainterH; ClipRect: TRect);
var
  IntersectRgn, Rgn: QRegionH;
  Matrix: QWMatrixH;
begin
  if QPainter_hasWorldXForm(DC) then
  begin
    Matrix := QPainter_worldMatrix(DC);
    QWMatrix_map(Matrix, PRect(@ClipRect), PRect(@ClipRect));
  end;

  IntersectRgn := QRegion_create(@ClipRect, QRegionRegionType_Rectangle);
  Rgn := QPainter_clipRegion(DC);
  if QRegion_isNull(Rgn) then
    Rgn := IntersectRgn
  else
    QRegion_intersect(Rgn, Rgn, IntersectRgn);
  QPainter_setClipRegion(DC, Rgn);
  QPainter_setClipping(DC, True);
  QRegion_destroy(IntersectRgn);
end;

{$IFDEF MSWINDOWS}
function GetWindowFromPainter(Painter: QPainterH): Cardinal;
var dc: HDC;
begin
  Result := 0;
  dc := QPainter_handle(Painter);
  if dc <> 0 then
    Result := Cardinal(WindowFromDC(dc));
end;
{$ELSE}
function GetWindowFromPainter(Painter: QPainterH): Cardinal;
var dev: QPaintDeviceH;
begin
  dev := QPainter_device(Painter);
  if dev <> nil then
    Result := Cardinal(QPaintDevice_handle(dev))
  else
    Result := 0;
end;
{$ENDIF MSWINDOWS}

function GetWidgetFromPainter(Painter: QPainterH): QWidgetH;
var wnd: Cardinal;
begin
  Result := nil;
  wnd := GetWindowFromPainter(Painter);
  if wnd <> 0 then
    Result := QWidget_find(wnd);
end;


const
// constants for Canvas.TextRect
  AlignLeft = 1 { $1 };
  AlignRight = 2 { $2 };
  AlignHCenter = 4 { $4 };
  AlignTop = 8 { $8 };
  AlignBottom = 16 { $10 };
  AlignVCenter = 32 { $20 };
  AlignCenter = 36 { $24 };
  SingleLine = 64 { $40 };
  DontClip = 128 { $80 };
  ExpandTabs = 256 { $100 };
  ShowPrefix = 512 { $200 };
  WordBreak = 1024 { $400 };
  ModifyString = 2048 { $800 };
  DontPrint = 4096 { $1000 };
  ClipPath = 8192 { $2000 };
  ClipName = 16386 { $4000 };
  CalcRect =  32768 { $8000 } ;

{$IFDEF LINUX}
const
  { ClxDrawText() Format Flags }
  DT_TOP        = 0;  // default
  DT_LEFT       = 0;  // default
  DT_CENTER     = 1;
  DT_RIGHT      = 2;
  DT_VCENTER    = 4;
  DT_BOTTOM     = 8;
  DT_WORDBREAK  = $10;
  DT_SINGLELINE = $20;
  DT_EXPANDTABS = $40;
  //DT_TABSTOP    = $80;
  DT_NOCLIP = $100;
  //DT_EXTERNALLEADING = $200;
  DT_CALCRECT   = $400;
  DT_NOPREFIX   = $800;
  //DT_INTERNAL   = $1000;
  //DT_HIDEPREFIX = $00100000;
  //DT_PREFIXONLY = $00200000;

  //DT_EDITCONTROL = $2000;
  DT_PATH_ELLIPSIS = $4000;
  DT_END_ELLIPSIS  = $8000;
  DT_ELLIPSIS = DT_END_ELLIPSIS;
  DT_MODIFYSTRING = $10000;
//  DT_RTLREADING = $20000;
//  DT_WORD_ELLIPSIS = $40000;

  { ClxExtTextOut() Format Flags }
  ETO_OPAQUE     = 2;
  ETO_CLIPPED    = 4;
  //ETO_GLYPH_INDEX = $10;
  ETO_RTLREADING = $80;  // ignored
  //ETO_NUMERICSLOCAL  = $400;
  //ETO_NUMERICSLATIN  = $800;
  //ETO_IGNORELANGUAGE = $1000;
  //ETO_PDY = $2000;

  { ShowWindow() Commands }
  SW_HIDE            = 0;
  SW_SHOWNORMAL      = 1;
  SW_NORMAL          = 1;
  SW_SHOWMINIMIZED   = 2;
  SW_SHOWMAXIMIZED   = 3;
  SW_MAXIMIZE        = 3;
  SW_SHOWNOACTIVATE  = 4;
  SW_SHOW            = 5;
  SW_MINIMIZE        = 6;
  SW_SHOWMINNOACTIVE = 7;
  SW_SHOWNA          = 8;
  SW_RESTORE         = 9;
  SW_SHOWDEFAULT     = 10;
  SW_MAX             = 10;
{$ENDIF LINUX}

function ClxDrawTextW(Canvas: TCanvas; var Caption: WideString; var R: TRect;
  Flags: Integer): Integer;
var
  Flgs: Word;
  Text: string;
begin
  Text := Caption;
  with Canvas do
  begin
    Flgs := 0;
    if Flags and DT_SINGLELINE <> 0 then
       Flgs := SingleLine;
    if Flags and DT_WORDBREAK <> 0 then
       Flgs := Flgs or WordBreak;
    if Flags and DT_EXPANDTABS <> 0 then
      Flgs := Flgs or ExpandTabs;
    if Flags and DT_NOPREFIX = 0 then
      Flgs := Flgs or ShowPrefix;
    if Flags and DT_RIGHT <> 0 then
      Flgs := Flgs or AlignRight
    else if Flags and DT_CENTER <> 0 then
      Flgs := Flgs or AlignHCenter
    else
      Flgs := Flgs or AlignLeft ; // default
    // vertical alignment
    if Flags and DT_BOTTOM <> 0 then
      Flgs := Flgs or AlignTop
    else if Flags and DT_VCENTER <> 0 then
      Flgs := Flgs or AlignVCenter
    else
      Flgs := Flgs or AlignTop;  // default

    if Flags and DT_CALCRECT <> 0 then
    begin
      TextExtent(Caption, R, flgs);
      Result := R.Bottom - R.Top;
      Exit;
    end;
    Canvas.TextRect(R, R.Left, R.Top, Text, Flgs);
    if Flags and DT_MODIFYSTRING <> 0 then
      Caption := Text;
  end;
  Result := 1;
end;

function RGB(r, g, b: Integer): TColor;
var
  C: QColorH;
begin
  C := QColor_create(r, g, b);
  Result := QColorColor(C);
  QColor_destroy(C);
end;


procedure DimBitmap(ABitmap: TBitmap; Value: integer);

  function NewColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
  var
    r, g, b: integer;
  begin
    if Value > 100 then Value := 100;
    clr := ColorToRGB(clr);
    r := Clr and $000000FF;
    g := (Clr and $0000FF00) shr 8;
    b := (Clr and $00FF0000) shr 16;

    r := r + Round((255 - r) * (value / 100));
    g := g + Round((255 - g) * (value / 100));
    b := b + Round((255 - b) * (value / 100));

{    ACanvas.Start;
    Result := Windows.GetNearestColor(HDC(QPainter_handle(ACanvas.Handle)), RGB(r, g, b));
    ACanvas.Stop;}
    Result := RGB(r, g, b);
  end;

var
  x, y: integer;
  LastColor1, LastColor2, Color: TColor;
begin
  if Value > 100 then Value := 100;
  LastColor1 := -1;
  LastColor2 := -1;

  ABitmap.Canvas.Start;
  try
    for y := 0 to ABitmap.Height - 1 do
      for x := 0 to ABitmap.Width - 1 do
      begin
        Color := ABitmap.Canvas.Pixels[x, y];
        if Color = LastColor1 then
          ABitmap.Canvas.Pixels[x, y] := LastColor2
        else
        begin
          LastColor2 := NewColor(ABitmap.Canvas, Color, Value);
          ABitmap.Canvas.Pixels[x, y] := LastColor2;
          LastColor1 := Color;
        end;
      end;
  finally
    ABitmap.Canvas.Stop;
  end;
end;

procedure GrayBitmap(ABitmap: TBitmap; Value: integer);

  function GrayColor(Clr: TColor; Value: integer): TColor;
  var r, g, b, avg: integer;
  begin
    clr := ColorToRGB(clr);
    r := Clr and $000000FF;
    g := (Clr and $0000FF00) shr 8;
    b := (Clr and $00FF0000) shr 16;

    Avg := (r + g + b) div 3;
    Avg := Avg + Value;

    if Avg > 240 then Avg := 240;
    Result := RGB(Avg, Avg, Avg);
  end;

var
  x, y: integer;
  LastColor1, LastColor2, Color: TColor;
begin
  LastColor1 := 0;
  LastColor2 := 0;

  ABitmap.Canvas.Start;
  try
    for y := 0 to ABitmap.Height - 1 do
      for x := 0 to ABitmap.Width - 1 do
      begin
        Color := ABitmap.Canvas.Pixels[x, y];
        if Color = LastColor1 then
          ABitmap.Canvas.Pixels[x, y] := LastColor2
        else
        begin
          LastColor2 := GrayColor(Color, Value);
          ABitmap.Canvas.Pixels[x, y] := LastColor2;
          LastColor1 := Color;
        end;
      end;
  finally
    ABitmap.Canvas.Stop;
  end;
end;

procedure DrawBitmapShadow(Bmp: TBitmap; Canvas: TCanvas; X, Y: integer;
  ShadowColor: TColor);
var
  BX, BY: integer;
  TransparentColor: TColor;
begin
  Bmp.Canvas.Start;
  try
    TransparentColor := Bmp.Canvas.Pixels[0, Bmp.Height - 1];
    for BY := 0 to Bmp.Height - 1 do
      for BX := 0 to Bmp.Width - 1 do
        if Bmp.Canvas.Pixels[BX, BY] <> TransparentColor then
          Canvas.Pixels[X + BX, Y + BY] := ShadowColor;
  finally
    Bmp.Canvas.Stop;
  end;
end;

function PrivateApp: TPrivateApplication;
begin
  Result := TPrivateApplication(Application);
end;

// ------- BEGIN Memory manipulation functions ----------

function WriteProtectedMemory(BaseAddress, Buffer: Pointer;
  Size: Cardinal; out WrittenBytes: Cardinal): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := WriteProcessMemory(GetCurrentProcess, BaseAddress, Buffer, Size, WrittenBytes);
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
var
  AlignedAddress: Cardinal;
  PageSize, ProtectSize: Cardinal;
begin
  Result := False;
  WrittenBytes := 0;

  PageSize := Cardinal(getpagesize);
  AlignedAddress := Cardinal(BaseAddress) and not (PageSize - 1); // start memory page
  // get the number of needed memory pages
  ProtectSize := PageSize;
  while Cardinal(BaseAddress) + Size > AlignedAddress + ProtectSize do
    Inc(ProtectSize, PageSize);

  if mprotect(Pointer(AlignedAddress), ProtectSize,
       PROT_READ or PROT_WRITE or PROT_EXEC) = 0 then // obtain write access
  begin
    try
      Move(Buffer^, BaseAddress^, Size); // replace code
      Result := True;
      WrittenBytes := Size;
    finally
      // Is there any function that returns the current page protection?
//    mprotect(p, ProtectSize, PROT_READ or PROT_EXEC); // lock memory page
    end;
  end;
end;
{$ENDIF LINUX}

function ReadProtectedMemory(BaseAddress, Buffer: Pointer;
  Size: Cardinal; out ReadBytes: Cardinal): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := ReadProcessMemory(GetCurrentProcess, BaseAddress, Buffer, Size, ReadBytes);
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
begin
  Move(BaseAddress^, Buffer^, Size); // read data
  Result := True;
  ReadBytes := Size;
end;
{$ENDIF LINUX}

procedure CodeRedirect(Proc: Pointer; NewProc: Pointer; out Data: TRedirectCode);
type
  TRelocationRec = packed record
    Jump: Word;
    Address: PPointer;
  end;

var
  Code: TRedirectCode;
  Relocation: TRelocationRec;
  n: Cardinal;
begin
  ReadProtectedMemory(Proc, @Data, 5, n);
  if Data.Jump = $ff then // Proc is in an dll or package
  begin
    if not ReadProtectedMemory(Proc, @Relocation, SizeOf(TRelocationRec), n) then
      RaiseLastOSError;
    Data.RealProc := Relocation.Address^;
    Proc := Data.RealProc;
    if not ReadProtectedMemory(Proc, @Data, 5, n) then
      RaiseLastOSError;
  end
  else
    Data.RealProc := Proc;
  Code.Jump := $e9;
  Code.Offset := Integer(NewProc) - Integer(Proc) - 5;
  if not WriteProtectedMemory(Proc, @Code, 5, n) then
    RaiseLastOSError;
end;

procedure CodeRestore(Proc: Pointer; const Data: TRedirectCode);
var n: Cardinal;
begin
  if Data.RealProc = nil then Exit;
  WriteProtectedMemory(Data.RealProc, @Data, 5, n);
end;

procedure ReplaceVmtField(AClass: TClass; OldProc, NewProc: Pointer);
type
  PVmt = ^TVmt;
  TVmt = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;
  TRelocationRec = packed record
    Jump: Byte;
    Res: Byte;
    Address: PPointer;
  end;
var
  I: Integer;
  Vmt: PVmt;
  Relocation: TRelocationRec;
  n: Cardinal;
begin
  if not ReadProtectedMemory(OldProc, @Relocation, SizeOf(TRelocationRec), n) then
    RaiseLastOSError;
  if Relocation.Jump = $ff then // Proc is in an dll or package
    OldProc := Relocation.Address^;

  if not ReadProtectedMemory(NewProc, @Relocation, SizeOf(TRelocationRec), n) then
    RaiseLastOSError;
  if Relocation.Jump = $ff then // Proc is in an dll or package
    NewProc := Relocation.Address^;

  I := 0;
  Vmt := Pointer(AClass);
  while Vmt[I] <> nil do
  begin
    if Vmt[I] = OldProc then
    begin
      WriteProtectedMemory(@Vmt[I], @NewProc, SizeOf(NewProc), n);
      Break;
    end;
    Inc(I);
  end;
end;


function GetSubCallAddress(Proc: Pointer; CallNum: Integer): Pointer;
type
  TRelocationRec = packed record
    Jump: Word;
    Address: PPointer;
  end;
var
  Code: TRedirectCode;
  Relocation: TRelocationRec;
  n: Cardinal;
  P: PByte;
begin
  ReadProtectedMemory(Proc, @Code, 5, n);
  if Code.Jump = $ff then // Proc is in an dll or package
  begin
    if not ReadProtectedMemory(Proc, @Relocation, SizeOf(TRelocationRec), n) then
      RaiseLastOSError;
    Proc := Relocation.Address^;
  end;

  P := Proc;

 // A very, very small disassembler that is not really correct but it does its
 // job.
  while P^ <> $C3 do // "ret"
  begin
    case P^ of
      $3B: Inc(P, 2); // CMP reg,reg
      $53: Inc(P);    // PUSH EBX
      $55: Inc(P);    // PUSH EBP
      $5B: Inc(P);    // POP EBX
      $5D: Inc(P);    // POP EBP
      $83:
        begin
          Inc(P);
          if P^ = $CA then
            Inc(P, 2);
        end;

      $8B: Inc(P, 2); // MOV reg,reg

      $E8: // CALL xxxx
        begin
          Dec(CallNum);
          Inc(P);
          if CallNum = 0 then
          begin
            Result := Pointer(Integer(P) - 1 + PInteger(P)^ + 5);
            Exit;
          end;
          Inc(P, 4);
        end;
      $E9: Inc(P, 4 + 1); // JMP xxxx
    else
      Inc(P);
    end;
  end;
  Result := nil;
end;

// ------- END Memory manipulation functions ----------

var
  OrgQFrame_drawFrame: procedure(Handle: QWidgetH; Painter: QPainterH); cdecl;

procedure HookedQFrame_drawFrame(Handle: QWidgetH; Painter: QPainterH); cdecl;
var
  Control: TWidgetControl;
  DrawRect: TRect;
  Details: TThemedElementDetails;
  MaskSingle, MaskSunken3D: Integer;
begin
  Control := FindControl(Handle);
  if not (Control is TFrameControl) or
     not (TOpenFrameControl(Control).BorderStyle in [bsSingle, bsSunken3d]) then
  begin
    if QObject_inherits(QObjectH(Handle), 'QFrame') then
    begin
      MaskSingle := Integer(QFrameShape_Box) or Integer(QFrameShadow_Plain);
      MaskSunken3D := Integer(QFrameShape_WinPanel) or Integer(QFrameShadow_Sunken);
      if ((QFrame_frameStyle(QFrameH(Handle)) and MaskSunken3D = MaskSunken3D)) or
         ((QFrame_frameStyle(QFrameH(Handle)) and MaskSingle = MaskSingle)) then
      begin
        QWidget_geometry(Handle, @DrawRect);
        OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
        QPainter_save(Painter);
        try
          with DrawRect do
            QExcludeClipRect(Painter, Left + 2, Top + 2, Right - 2, Bottom - 2);
          Details := ThemeServices.GetElementDetails(teEditTextNormal);
          ThemeServices.DrawElement(Painter, Details, DrawRect);
        finally
          QPainter_restore(Painter);
        end;
        Exit;
      end;
    end;
    CodeRestore(@OrgQFrame_drawFrame, ThemedStyle.FCodeQFrame_drawFrame);
    try
      OrgQFrame_drawFrame(Handle, Painter);
    finally
      CodeRedirect(@OrgQFrame_drawFrame, @HookedQFrame_drawFrame, ThemedStyle.FCodeQFrame_drawFrame);
    end;
    Exit;
  end;

  if Control is TFrameControl then
    ThemeServices.PaintBorder(Control, False, Painter)
end;

function GetAddress_QFrame_drawFrame(Handle: QWidgetH): Pointer;
const
{$MESSAGE WARN 'Qt 2.3 related code'}
{$IFDEF MSWINDOWS}
  VMT_OFFSET = $08;
  VMT_OFFSET_QFrame_drawFrame = $67;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  VMT_OFFSET = $20;
  VMT_OFFSET_QFrame_drawFrame = $66;
{$ENDIF LINUX}
type
  QVmtExtraData = packed record
    {$IFDEF LINUX}
    {$MESSAGE WARN 'gcc 2.95 related code'}
     { gcc 2.95 VMT structure }
    Reserved: Pointer;
    RTTIFunc: Pointer;
    {$ENDIF LINUX}
  end;

  QVmtH = ^QVmt;
  QVmt = packed record
    ExtraData: QVmtExtraData;
    Entry: packed array[0..65535] of Pointer;
  end;

  function GetQVmt(Handle: QWidgetH): QVmtH;
  begin
    Result := QVmtH(PInteger(Cardinal(Handle) + VMT_OFFSET)^);
  end;

begin
  Result := GetQVmt(Handle).Entry[VMT_OFFSET_QFrame_drawFrame];
end;

procedure DrawThemedFlatButtonFace(Canvas: TCanvas; const Client: TRect;
  IsDown: Boolean; DrawContent: Boolean; IsMouseOverFlag: Integer = 0);
var
  Details: TThemedElementDetails;
  ToolBtn: TThemedToolBar;
  PaintRect, ExcludeRect: TRect;
begin
  ToolBtn := ttbButtonNormal;

  if (IsMouseOverFlag = 0) and (Canvas is TControlCanvas) then
    if IsMouseOver(TControlCanvas(Canvas).Control) then
      ToolBtn := ttbButtonHot;
  if IsMouseOverFlag = 1 then
    ToolBtn := ttbButtonHot;
  if IsDown then ToolBtn := ttbButtonPressed;
  if Canvas is TControlCanvas then
    if not TControlCanvas(Canvas).Control.Enabled then
      ToolBtn := ttbButtonDisabled;

  if ToolBtn = ttbButtonNormal then Exit;

  PaintRect := Client;

  Details := ThemeServices.GetElementDetails(ToolBtn);
  Canvas.Start;
  try
    QPainter_save(Canvas.Handle);
    try
      ExcludeRect := PaintRect;
      InflateRect(ExcludeRect, -2, -2);
      if not DrawContent then
      begin
        with ExcludeRect do
          QExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
      end;
      ThemeServices.DrawElement(Canvas, Details, PaintRect);
    finally
      QPainter_restore(Canvas.Handle);
    end;
  finally
    Canvas.Stop;
  end;
  Canvas.SetClipRect(ExcludeRect);
end;

function DrawButtonFaceEx(Canvas: TCanvas; const Client: TRect;
  BevelWidth: Integer; IsDown, IsFocused: Boolean; Flat: Boolean = False;
  IsMouseOverFlag: Integer = 0;
  FillColor: TColor = clButton; FillStyle: TBrushStyle = bsSolid): TRect;
  forward;

function DrawThemedButtonFace(Canvas: TCanvas; const Client: TRect;
  BevelWidth: Integer; IsDown, IsFocused: Boolean; Flat: Boolean = False;
  FillColor: TColor = clButton; FillStyle: TBrushStyle = bsSolid): TRect;
begin
  Result := DrawButtonFaceEx(Canvas, Client, BevelWidth, IsDown, IsFocused, Flat,
    0, FillColor, FillStyle);
end;

function DrawButtonFaceEx(Canvas: TCanvas; const Client: TRect;
  BevelWidth: Integer; IsDown, IsFocused: Boolean; Flat: Boolean = False;
  IsMouseOverFlag: Integer = 0;
  FillColor: TColor = clButton; FillStyle: TBrushStyle = bsSolid): TRect;
var
  Details: TThemedElementDetails;
  Button: TThemedButton;
begin
  if FillColor <> clButton then
  begin
    CodeRestore(@DrawButtonFace, ThemedStyle.FCodeDrawButtonFace);
    try
      Result := DrawButtonFace(Canvas, Client, BevelWidth, IsDown, IsFocused, Flat, FillColor, FillStyle);
    finally
      CodeRedirect(@DrawButtonFace, @DrawThemedButtonFace, ThemedStyle.FCodeDrawButtonFace);
    end;
    Exit;
  end;

  Result := Client;
  if not Flat then
  begin
    Button := tbPushButtonNormal;
    if IsFocused then Button := tbPushButtonDefaulted;

    if (IsMouseOverFlag = 0) and (Canvas is TControlCanvas) then
      if IsMouseOver(TControlCanvas(Canvas).Control) then
        Button := tbPushButtonHot;
    if IsMouseOverFlag = 1 then
      Button := tbPushButtonHot;
    if IsDown then Button := tbPushButtonPressed;
    if Canvas is TControlCanvas then
      if not TControlCanvas(Canvas).Control.Enabled then
        Button := tbPushButtonDisabled;

    if Flat and (Button = tbPushButtonNormal) then Exit;

    Details := ThemeServices.GetElementDetails(Button);
    ThemeServices.DrawElement(Canvas, Details, Result);
  end
  else
    DrawThemedFlatButtonFace(Canvas, Client, IsDown, True);
end;

procedure DrawThemedEdge(Canvas: TCanvas; R: TRect; EdgeInner, EdgeOuter: TEdgeStyle;
  EdgeBorders: TEdgeBorders);
begin
  if Canvas is TControlCanvas then
  begin
    if TControlCanvas(Canvas).Control is TSpeedButton then
    begin
      DrawThemedFlatButtonFace(Canvas, R, EdgeOuter = esLowered, True);
      Exit;
    end;
  end;

  CodeRestore(@DrawEdge, ThemedStyle.FCodeDrawEdge);
  try
    DrawEdge(Canvas, R, EdgeInner, EdgeOuter, EdgeBorders);
  finally
    CodeRedirect(@DrawEdge, @DrawThemedEdge, ThemedStyle.FCodeDrawEdge);
  end;
end;


var
 OrgqDrawShadePanel: procedure(p: QPainterH; r: PRect; g: QColorGroupH;
   sunken: Boolean; lineWidth: Integer; fill: QBrushH); cdecl;

procedure DrawThemedShadePanel(p: QPainterH; r: PRect; g: QColorGroupH;
  sunken: Boolean; lineWidth: Integer; fill: QBrushH); cdecl;
var
  Details: TThemedElementDetails;
  ClipRect: TRect;
begin
  if sunken and (lineWidth = 1) and (r <> nil) then
  begin
    QPainter_save(p);
    try
      ClipRect := r^;
      with ClipRect do
        QExcludeClipRect(p, Left + 2, Top + 2, Right - 2, Bottom - 2);
      Details := ThemeServices.GetElementDetails(teEditTextNormal);
      ThemeServices.DrawElement(p, Details, r^);
    finally
      QPainter_restore(p);

      with ClipRect do
      begin
       // exclude 2 pixel from each side
        QExcludeClipRect(p, Left, Top, Left + 2, Bottom);
        QExcludeClipRect(p, Right, Top, Right - 2, Bottom);
        QExcludeClipRect(p, Left, Top, Right, Top + 2);
        QExcludeClipRect(p, Left, Bottom, Right, Bottom - 2);
      end;
    end;
  end
  else
  begin
    try
      CodeRestore(@OrgqDrawShadePanel, ThemedStyle.FCodeDrawShadePanel);
      OrgqDrawShadePanel(p, r, g, sunken, lineWidth, fill);
    finally
      CodeRedirect(@OrgqDrawShadePanel, @DrawThemedShadePanel, ThemedStyle.FCodeDrawShadePanel);
    end;
  end;
end;

{ TThemedStyle }

constructor TThemedStyle.Create;
begin
  if ThemedStyle <> nil then
  begin
    Free;
    Exit;
  end;
  ThemedStyle := Self;
  inherited Create;
  FControlInfo := TStringList.Create;
  FNotifyObject := TThemedStyleNotify.Create(nil);

  Application.Style := Self;
  Initialize;

  FMenusEnabled := True;
end;

destructor TThemedStyle.Destroy;
var I: Integer;
begin
  try
    Finalize;
  finally
    ThemedStyle := nil;
    for I := 0 to FControlInfo.Count - 1 do
      TComponent(FControlInfo.Objects[I]).RemoveFreeNotification(FNotifyObject);
    FControlInfo.Free;
    FNotifyObject.Free;
  end;
  inherited Destroy;
end;

procedure TThemedStyle.DrawPushButtonHook(btn: QPushButtonH; p: QPainterH;
  var Stage: Integer);
var
  Canvas: TCanvas;
  Source: TObject;
begin
  if (btn = nil) or (p = nil) then Exit;
  try
    Source := FindControl(btn);
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := p;
      Canvas.Start(False);
      if Source <> nil then
      begin
        case Stage of
          DrawStage_Pre:
            if DoBeforeDrawButton(Source, Canvas) then
              Stage := DrawStage_DefaultDraw;
          DrawStage_Post:
            DoAfterDrawButton(Source, Canvas);
        end;
      end
      else
      begin
        case Stage of
          DrawStage_Pre:
            if DoBeforeDrawButtonWidget(btn, Canvas) then
              Stage := DrawStage_DefaultDraw;
          DrawStage_Post:
            DoAfterDrawButtonWidget(btn, Canvas);
        end;
      end;
      Canvas.Stop;
    finally
      Canvas.Free;
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TThemedStyle.DrawPopupMenuItemHook(p: QPainterH;
  checkable: Boolean; maxpmw, tab: Integer; mi: QMenuItemH; itemID: Integer;
  act, enabled: Boolean; x, y, w, h: Integer; var Stage: Integer);
var
//  Source: TObject;
  Canvas: TCanvas;
  R: TRect;
begin
  try
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := p;
      R := Rect(x, y, x + w, y + h);
      Canvas.Start(False);
{      Source := FindObject(QObjectH(mi));
      if Source <> nil then
        DoDrawMenuItem(Source, Canvas, act, enabled, R, checkable, maxpmw, tab, Stage)
      else}
        DoDrawMenuItemObject(mi, Canvas, act, enabled, R, checkable, maxpmw, tab, Stage);
      Canvas.Stop;
    finally
      Canvas.Free;
    end;
  except
    Application.HandleException(Self);
  end;
end;

{$IFDEF LINUX}
// MDI Child title buttons under Linux. Under Windows this is done in EvDrawHeaderSection
procedure TThemedStyle.DrawQToolButton(Canvas: TCanvas; R: TRect; Down, Focused: Boolean);
begin
  InflateRect(R, 2, 2);
  DrawThemedButtonFace(Canvas, R, 2, False, Focused); // background
  InflateRect(R, -1, -1);
  DrawThemedButtonFace(Canvas, R, 2, Down, Focused);
end;

procedure TThemedStyle.DoDrawToolButtonHook(p: QPainterH; x, y, w, h: Integer; g: QColorGroupH;
  sunken: Boolean; fill: QBrushH);
var
  Canvas: TCanvas;
  R: TRect;
begin
  try
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := p;
      R := Rect(x, y, x + w, y + h);
      Canvas.Start(False);
        DrawQToolButton(Canvas, R, sunken, False);
      Canvas.Stop;
    finally
      Canvas.Free;
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TThemedStyle.DoDrawToolButton2Hook(btn: QToolButtonH; p: QPainterH);
var
  Canvas: TCanvas;
  R: TRect;
begin
  if btn = nil then
    Exit;

  Canvas := TCanvas.Create;
  try
    Canvas.Handle := p;
    QWidget_geometry(btn, @R);
    Canvas.Start(False);
      DrawQToolButton(Canvas, R, QButton_isDown(btn), QWidget_isActiveWindow(btn));
    Canvas.Stop;
  finally
    Canvas.Free;
  end;
end;
{$ENDIF LINUX}

procedure TThemedStyle.EvBeforeDrawButton(Sender, Source: TObject; Canvas: TCanvas;
  var DefaultDraw: Boolean);
var
  R: TRect;
begin
  R := TWidgetControl(Source).ClientRect;
  DefaultDraw := False;
  ThemeServices.DrawParentBackground(TWidgetControl(Source).Handle, Canvas, nil, False, @R);
end;

procedure TThemedStyle.EvAfterDrawButton(Sender, Source: TObject; Canvas: TCanvas);
var
  Details: TThemedElementDetails;
  Button: TThemedButton;
  Down: Boolean;
  R: TRect;
  Control: TWidgetControl;
begin
  if Source is TButton then
    Down := TButton(Source).Down
  else if Source is TSpinEdit then
  begin
    Details := ThemeServices.GetElementDetails(tsArrowBtnUpNormal);
    R := TControl(Source).ClientRect;
    R.Bottom := R.Top + (R.Bottom - R.Top) div 2;
    ThemeServices.DrawElement(Canvas, Details, R);

    Details := ThemeServices.GetElementDetails(tsArrowBtnDownNormal);
    R := TControl(Source).ClientRect;
    R.Top := R.Top + (R.Bottom - R.Top) div 2;
    ThemeServices.DrawElement(Canvas, Details, R);

    Exit;
  end
  else
    Down := False;

  Control := TWidgetControl(Source);
  Button := tbPushButtonNormal;
  if ((Source is TButton) and (TButton(Source).Default)) or
     (Control.Focused) then Button := tbPushButtonDefaulted;
  if IsMouseOver(Control) then Button := tbPushButtonHot;
  if Down then Button := tbPushButtonPressed;
  if not Control.Enabled then Button := tbPushButtonDisabled;

  Details := ThemeServices.GetElementDetails(Button);
  ThemeServices.DrawElement(Canvas, Details, Control.ClientRect);
end;

function TThemedStyle.DoBeforeDrawButtonWidget(btn: QPushButtonH;
  Canvas: TCanvas): Boolean;
var R: TRect;
begin
  Result := False;
  QWidget_geometry(btn, @R);
  OffsetRect(R, -R.Left, -R.Top);
  ThemeServices.DrawParentBackground(btn, Canvas, nil, False, @R);
end;

procedure TThemedStyle.DoAfterDrawButtonWidget(btn: QPushButtonH; Canvas: TCanvas);
var
  Details: TThemedElementDetails;
  Button: TThemedButton;
  Down: Boolean;
  R: TRect;
begin
  QWidget_geometry(btn, @R);
  OffsetRect(R, -R.Left, -R.Top);

  Down := QButton_isDown(btn);

  Button := tbPushButtonNormal;
  if QPushButton_isDefault(btn) then Button := tbPushButtonDefaulted;
//  if IsMouseOver(QWidgetH(btn), nil) then Button := tbPushButtonHot;
  if Down then Button := tbPushButtonPressed;
  if not QWidget_isEnabled(btn) then Button := tbPushButtonDisabled;

  Details := ThemeServices.GetElementDetails(Button);
  ThemeServices.DrawElement(Canvas, Details, R);
end;

procedure TThemedStyle.EvDrawComboButton(Sender: TObject; Canvas: TCanvas; const Rect: TRect;
  Sunken, ReadOnly, Enabled: Boolean; var DefaultDraw: Boolean);
var
  Details: TThemedElementDetails;
  ComboBox: TThemedComboBox;
  R: TRect;
begin
  DefaultDraw := False;

  Details := ThemeServices.GetElementDetails(teEditTextNormal);
  ThemeServices.DrawElement(Canvas, Details, Rect);

  ComboBox := tcDropDownButtonNormal;
  if Sunken then ComboBox := tcDropDownButtonPressed;
  if (not Enabled) then ComboBox := tcDropDownButtonDisabled;
  Details := ThemeServices.GetElementDetails(ComboBox);
  R := Rect;R.Left := R.Right - 19;
  InflateRect(R, -1, -1);
  ThemeServices.DrawElement(Canvas, Details, R);
end;

procedure TThemedStyle.EvDrawCheck(Sender: TObject; Canvas: TCanvas; const Rect: TRect;
  Checked, Grayed, Down, Enabled: Boolean; var DefaultDraw: Boolean);
var
  Details: TThemedElementDetails;
  Button: TThemedButton;
begin
  DefaultDraw := False;

  if Grayed then
  begin
    Button := tbCheckBoxMixedNormal;
    if Down then Button := tbCheckBoxMixedPressed;
    if not Enabled then Button := tbCheckBoxMixedDisabled;
  end
  else
  if Checked then
  begin
    Button := tbCheckBoxCheckedNormal;
    if Down then Button := tbCheckBoxCheckedPressed;
    if not Enabled then Button := tbCheckBoxCheckedDisabled;
  end
  else
  begin
    Button := tbCheckBoxUncheckedNormal;
    if Down then Button := tbCheckBoxUncheckedPressed;
    if not Enabled then Button := tbCheckBoxUncheckedDisabled;
  end;

  Details := ThemeServices.GetElementDetails(Button);
  ThemeServices.DrawElement(Canvas, Details, Rect);
end;

// TRadioButton
procedure TThemedStyle.EvDrawRadio(Sender: TObject; Canvas: TCanvas;
  const Rect: TRect; Checked, Down, Enabled: Boolean; var DefaultDraw: Boolean);
var
  Details: TThemedElementDetails;
  Button: TThemedButton;
begin
  DefaultDraw := False;

  Canvas.Brush.Color := clBackground;
  Canvas.FillRect(Rect);

  if Checked then
  begin
    Button := tbRadioButtonCheckedNormal;
    if Down then Button := tbRadioButtonCheckedPressed;
    if not Enabled then Button := tbRadioButtonCheckedDisabled;
  end
  else
  begin
    Button := tbRadioButtonUncheckedNormal;
    if Down then Button := tbRadioButtonUncheckedPressed;
    if not Enabled then Button := tbRadioButtonUncheckedDisabled;
  end;

  Details := ThemeServices.GetElementDetails(Button);
  ThemeServices.DrawElement(Canvas, Details, Rect);
end;

procedure TThemedStyle.EvDrawRadioMask(Sender: TObject; Canvas: TCanvas; const
  Rect: TRect; Checked: Boolean);
var
  R: TRect;
begin
  R := Rect;
  Dec(R.Left);
  Dec(R.Top);
  {$IFDEF USE_WHITE_RADIOMASK}
  Canvas.Brush.Color := clWhite; // needed for TRadioGroup
  {$ELSE}
  Canvas.Brush.Color := clBlack;
  {$ENDIF USE_WHITE_RADIOMASK}
  Canvas.Ellipse(R);
end;

// ******
procedure TThemedStyle.EvDrawFrame(Sender: TObject; Canvas: TCanvas;
  const Rect: TRect; Sunken: Boolean; LineWidth: Integer;
  var DefaultDraw: Boolean);
var
  Details: TThemedElementDetails;
begin
  DefaultDraw := False;
  Details := ThemeServices.GetElementDetails(teEditTextNormal);
  ThemeServices.DrawElement(Canvas, Details, Rect);
end;

procedure TThemedStyle.EvDrawScrollBar(Sender: TObject;
  ScrollBar: QScrollBarH; Canvas: TCanvas; const Rect: TRect;
  SliderStart, SliderLength, ButtonSize: Integer; Controls: TScrollBarControls;
  DownControl: TScrollBarControl; var DefaultDraw: Boolean);

  procedure DrawItem(const R: TRect; Scroll: TThemedScrollBar);
  var
    Details: TThemedElementDetails;
  begin
    if QScrollBar_orientation(ScrollBar) = Orientation_Horizontal then
    begin
      case Scroll of
        tsArrowBtnUpNormal: Scroll := tsArrowBtnLeftNormal;
        tsArrowBtnUpHot: Scroll := tsArrowBtnLeftHot;
        tsArrowBtnUpPressed: Scroll := tsArrowBtnLeftPressed;
        tsArrowBtnUpDisabled: Scroll := tsArrowBtnLeftDisabled;

        tsArrowBtnDownNormal: Scroll := tsArrowBtnRightNormal;
        tsArrowBtnDownHot: Scroll := tsArrowBtnRightHot;
        tsArrowBtnDownPressed: Scroll := tsArrowBtnRightPressed;
        tsArrowBtnDownDisabled: Scroll := tsArrowBtnRightDisabled;

        tsThumbBtnHorzNormal: Scroll := tsThumbBtnVertNormal;
        tsThumbBtnHorzHot: Scroll := tsThumbBtnVertHot;
        tsThumbBtnHorzPressed: Scroll := tsThumbBtnVertPressed;
        tsThumbBtnHorzDisabled: Scroll := tsThumbBtnVertDisabled;
      end;
    end;
    Details := ThemeServices.GetElementDetails(Scroll);
    ThemeServices.DrawElement(Canvas, Details, R);
  end;

var
  Details: TThemedElementDetails;
  TopRect, BottomRect, ThumbRect, FillRect, FillRectTop, FillRectBottom: TRect;
  Scroll: TThemedScrollBar;
  WindowTop, WindowBottom: TThemedScrollBar;
begin
  if ScrollBar = nil then Exit;
  DefaultDraw := False;

  if QScrollBar_orientation(ScrollBar) = Orientation_Horizontal then
  begin
    TopRect := Types.Rect(0, 0, QWidget_height(ScrollBar), ButtonSize);
    BottomRect := Types.Rect(QWidget_width(ScrollBar) - ButtonSize, 0, QWidget_width(ScrollBar), QWidget_height(ScrollBar));
    ThumbRect := Types.Rect(SliderStart, 0, SliderStart + SliderLength, QWidget_height(ScrollBar));
    FillRect := Types.Rect(ButtonSize - 1, 0, QWidget_width(ScrollBar) - ButtonSize, QWidget_height(ScrollBar));
    FillRectTop := FillRect;FillRectTop.Right := ThumbRect.Left;
    FillRectBottom := FillRect;FillRectBottom.Left := ThumbRect.Right;

    if not QWidget_isEnabled(ScrollBar) then WindowTop := tsUpperTrackHorzDisabled
    else if DownControl = sbcAddPage then WindowTop := tsUpperTrackHorzPressed
    else if IsMouseOver(QWidgetH(ScrollBar), @BottomRect) then WindowTop := tsUpperTrackHorzHot
    else WindowTop := tsUpperTrackHorzNormal;

    if not QWidget_isEnabled(ScrollBar) then WindowBottom := tsLowerTrackHorzDisabled
    else if DownControl = sbcSubPage then WindowBottom := tsLowerTrackHorzPressed
    else if IsMouseOver(QWidgetH(ScrollBar), @BottomRect) then WindowBottom := tsLowerTrackHorzHot
    else WindowBottom := tsLowerTrackHorzNormal;
  end
  else
  begin
    TopRect := Types.Rect(0, 0, QWidget_width(ScrollBar), ButtonSize);
    BottomRect := Types.Rect(0, QWidget_height(ScrollBar) - ButtonSize, QWidget_width(ScrollBar), QWidget_height(ScrollBar));
    ThumbRect := Types.Rect(0, SliderStart, QWidget_width(ScrollBar), SliderStart + SliderLength);
    FillRect := Types.Rect(0, ButtonSize - 1, QWidget_width(ScrollBar), QWidget_height(ScrollBar) - ButtonSize);
    FillRectTop := FillRect;FillRectTop.Bottom := ThumbRect.Top;
    FillRectBottom := FillRect;FillRectBottom.Top := ThumbRect.Bottom;

    if not QWidget_isEnabled(ScrollBar) then WindowTop := tsUpperTrackVertDisabled
    else if DownControl = sbcAddPage then WindowTop := tsUpperTrackVertPressed
    else if IsMouseOver(QWidgetH(ScrollBar), @BottomRect) then WindowTop := tsUpperTrackVertHot
    else WindowTop := tsUpperTrackVertNormal;

    if not QWidget_isEnabled(ScrollBar) then WindowBottom := tsLowerTrackVertDisabled
    else if DownControl = sbcSubPage then WindowBottom := tsLowerTrackVertPressed
    else if IsMouseOver(QWidgetH(ScrollBar), @BottomRect) then WindowBottom := tsLowerTrackVertHot
    else WindowBottom := tsLowerTrackVertNormal;
  end;

 { sbcAddPage, sbcSubPage }
  Details := ThemeServices.GetElementDetails(WindowBottom);
  ThemeServices.DrawElement(Canvas, Details, FillRectTop);

  Details := ThemeServices.GetElementDetails(WindowTop);
  ThemeServices.DrawElement(Canvas, Details, FillRectBottom);

  if not QWidget_isEnabled(ScrollBar) then Scroll := tsArrowBtnUpDisabled
  else if DownControl = sbcSubButton then Scroll := tsArrowBtnUpPressed
  else if IsMouseOver(QWidgetH(ScrollBar), @TopRect) then Scroll := tsArrowBtnUpHot
  else Scroll := tsArrowBtnUpNormal;
  DrawItem(TopRect, Scroll);

  if not QWidget_isEnabled(ScrollBar) then Scroll := tsArrowBtnDownDisabled
  else if DownControl = sbcAddButton then Scroll := tsArrowBtnDownPressed
  else if IsMouseOver(QWidgetH(ScrollBar), @BottomRect) then Scroll := tsArrowBtnDownHot
  else Scroll := tsArrowBtnDownNormal;
  DrawItem(BottomRect, Scroll);

  if not QWidget_isEnabled(ScrollBar) then Scroll := tsThumbBtnVertDisabled
  else if DownControl = sbcSlider then Scroll := tsThumbBtnVertPressed
  else if IsMouseOver(QWidgetH(ScrollBar), @ThumbRect) then Scroll := tsThumbBtnVertHot
  else Scroll := tsThumbBtnVertNormal;
  DrawItem(ThumbRect, Scroll);
end;

procedure TThemedStyle.EvDrawTrackBar(Sender: TObject; Canvas: TCanvas; const Rect: TRect;
  Horizontal, TickAbove, TickBelow: Boolean; var DefaultDraw: Boolean);
var
  Details: TThemedElementDetails;
  TrackBar: TThemedTrackBar;
begin
  DefaultDraw := False;

  if Horizontal then
  begin
    if TickAbove and not TickBelow then
      TrackBar := ttbThumbTopNormal
    else if not TickAbove and TickBelow then
      TrackBar := ttbThumbBottomNormal
    else
      TrackBar := ttbThumbNormal
  end
  else
  begin
    if TickAbove and not TickBelow then
      TrackBar := ttbThumbLeftNormal
    else if not TickAbove and TickBelow then
      TrackBar := ttbThumbRightNormal
    else
      TrackBar := ttbThumbVertNormal;
  end;

  Details := ThemeServices.GetElementDetails(TrackBar);
  ThemeServices.DrawElement(Canvas, Details, Rect);
end;

procedure TThemedStyle.EvDrawTrackBarMask(Sender: TObject; Canvas: TCanvas; const Rect: TRect;
  Horizontal, TickAbove, TickBelow: Boolean; var DefaultDraw: Boolean);
var
  Bmp: TBitmap;
  X, Y: Integer;
  R: TRect;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Canvas.Brush.Color := clBlack;
    Bmp.Width := Rect.Right - Rect.Left;
    Bmp.Height := Rect.Bottom - Rect.Top;
    Bmp.Canvas.Start;
    try
      Bmp.Canvas.FillRect(Types.Rect(0, 0, Bmp.Width, Bmp.Height));
      R := Rect;
      OffsetRect(R, -R.Left, -R.Top);
      EvDrawTrackBar(Sender, Bmp.Canvas, R, Horizontal, TickAbove, TickBelow, DefaultDraw);

      for X := 0 to Bmp.Width - 1 do
        for Y := 0 to Bmp.Height - 1 do
          if Bmp.Canvas.Pixels[X, Y] <> 0 then
            Bmp.Canvas.Pixels[X, Y] := clBlack
          else
            Bmp.Canvas.Pixels[X, Y] := clWhite;
    finally
      Bmp.Canvas.Stop;
    end;

    Canvas.Draw(Rect.Left, Rect.Top, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TThemedStyle.EvDrawTrackBarGroove(Sender: TObject; Canvas: TCanvas;
  const Rect: TRect; Horizontal: Boolean; var DefaultDraw: Boolean);
const
  h = 10;
var
  Details: TThemedElementDetails;
  TrackBar: TThemedTrackBar;
  R: TRect;
begin
  DefaultDraw := False;
  TrackBar := ttbTrack;
  R := Rect;
  R.Top := R.Top + ((R.Bottom - R.Top) - h) div 2;
  R.Bottom := R.Top + h;
  Details := ThemeServices.GetElementDetails(TrackBar);
  ThemeServices.DrawElement(Canvas, Details, R);
end;

procedure TThemedStyle.EvDrawHeaderSection(Sender: TObject; Canvas: TCanvas;
  const Rect: TRect; Down: Boolean; var DefaultDraw: Boolean);
var
  Details: TThemedElementDetails;
  Header: TThemedHeader;
  wnd: Cardinal;
  PaintRect, R: TRect;
  Widget: QWidgetH;
begin
  PaintRect := Rect;
  Widget := nil;
  Canvas.Start;
  try
    wnd := GetWindowFromPainter(Canvas.Handle);
    if wnd <> 0 then
    begin
      {$IFDEF MSWINDOWS}
      GetWindowRect(HWND(wnd), R);
      {$ENDIF MSWINDOWS}
      Widget := QWidget_find(Cardinal(wnd));
      {$IFDEF LINUX}
      if Widget <> nil then
      begin
        QWidget_geometry(Widget, @R);
        QWidget_mapToGlobal(Widget, @R.TopLeft, @R.TopLeft);
        QWidget_mapToGlobal(Widget, @R.BottomRight, @R.BottomRight);
      end;
      {$ENDIF LINUX}
    end;

    // no default drawing from this point
    DefaultDraw := False;

    if Widget = nil then
    begin
     // MDI Childframe buttons
      Canvas.Brush.Color := clActiveHighlight;
      Canvas.FillRect(Rect);
      R := Rect;
      //InflateRect(R, 1, 1);
      DrawButtonFace(Canvas, R, 1, Down, False, False);
      Exit;
    end;


    if Down then
      Header := thHeaderItemPressed
    else
    if (wnd <> 0) and (PtInRect(R, Mouse.CursorPos)) and
      (PtInRect(Rect, Point(Mouse.CursorPos.X - R.Left, Mouse.CursorPos.Y - R.Top))) then
      Header := thHeaderItemHot
    else
      Header := thHeaderItemNormal;

{ // Windows XP does not draw the left and right orientated controls
    if QHeader_orientation(QHeaderH(Widget)) = Orientation_Vertical then
    begin
      Control := FindControl(Widget);
      if (Control <> nil) and (Control.Align = alLeft) then
        case Header of
          thHeaderItemNormal: Header := thHeaderItemLeftNormal;
          thHeaderItemPressed: Header := thHeaderItemLeftPressed;
          thHeaderItemHot: Header := thHeaderItemLeftHot;
        end
      else
        case Header of
          thHeaderItemNormal: Header := thHeaderItemRightNormal;
          thHeaderItemPressed: Header := thHeaderItemRightPressed;
          thHeaderItemHot: Header := thHeaderItemRightHot;
        end;
    end;}

    Details := ThemeServices.GetElementDetails(Header);
    ThemeServices.DrawElement(Canvas, Details, PaintRect);
  finally
    Canvas.Stop;
  end;
end;

procedure TThemedStyle.EvDrawMenuFrame(Sender: TObject;
  Canvas: TCanvas; const R: TRect; LineWidth: Integer;
  var DefaultDraw: Boolean);
var
  Details: TThemedElementDetails;
begin
  if not FMenusEnabled then Exit;
  DefaultDraw := False;

  Details := ThemeServices.GetElementDetails(ttPane);
  ThemeServices.DrawElement(Canvas, Details, R);
end;

procedure TThemedStyle.EvMenuItemHeight(Sender, Source: TObject; Checkable: Boolean;
  FontMetrics: QFontMetricsH; var Height: Integer);
begin
  if not FMenusEnabled then Exit;
  if TMenuItem(Source).Parent <> nil then
    if TMenuItem(Source).Parent.IndexOf(TMenuItem(Source)) = TMenuItem(Source).Parent.Count - 1 then
      Inc(Height, 2);
  if TMenuItem(Source).Caption = '-' then
    Inc(Height);
end;

type
  TQMenuItem = class
    RadioItem: Boolean;
    Checked: Boolean;
    Enabled: Boolean;
    Count: Integer;
    Caption: WideString;
    ShortCut: WideString;
  end;

procedure TThemedStyle.DoDrawMenuItemObject(Item: QMenuItemH; Canvas: TCanvas;
  Highlighted, Enabled: Boolean; const Rect: TRect; Checkable: Boolean;
  CheckMaxWidth, LabelWidth: Integer; var Stage: Integer);
const
  DrawSelect = True;
  FlatMenu = True;

const
  Color = $00FAFCFC {clWindow};
  IconBackColor = $00DEEDEF;  {clBtnFace}
  SelectColor = $00ECCFBD; {clHighlight}
  SelectBorderColor = clHighlight;
  MenuBarColor = clBtnFace;
  DisabledColor = $00A3B1B4;{clInactiveCaption}
  SeparatorColor = $00B3BEC1{clSilver};
  CheckedColor = clHighlight;
  SelectFontColor = clWindowText;// FFont.Color;
  CheckedAreaColor = SelectColor;
  CheckedAreaSelectColor = SelectColor;
  GrayLevel = 10;
  DimLevel = 30;
  IconWidth = 24;

  function GetShadeColor(clr: TColor; Value: Integer): TColor;
  var
    r, g, b: integer;
  begin
    clr := ColorToRGB(clr);
    r := Clr and $000000FF;
    g := (Clr and $0000FF00) shr 8;
    b := (Clr and $00FF0000) shr 16;

    r := (r - value);
    if r < 0 then r := 0;
    if r > 255 then r := 255;

    g := (g - value) + 2;
    if g < 0 then g := 0;
    if g > 255 then g := 255;

    b := (b - value);
    if b < 0 then b := 0;
    if b > 255 then b := 255;
    Result := RGB(r, g, b);
  end;

  procedure DrawCheckedItem(MenuItem: TQMenuItem; Selected, Enabled,
    HasImgLstBitmap: Boolean; ACanvas: TCanvas; CheckedRect: TRect);
  var
    X1, X2: integer;
  begin
    if MenuItem.RadioItem then
    begin
      if MenuItem.Checked then
      begin
        if Enabled then
        begin
          Canvas.Pen.color := SelectBorderColor;
          if Selected then
            Canvas.Brush.Color := CheckedAreaSelectColor
          else
            Canvas.Brush.Color := CheckedAreaColor;
        end
        else
          Canvas.Pen.Color := DisabledColor;

        Canvas.Brush.Style := bsSolid;
        if HasImgLstBitmap then
          Canvas.RoundRect(CheckedRect.Left, CheckedRect.Top,
            CheckedRect.Right, CheckedRect.Bottom,
            6, 6)
        else
          ACanvas.Ellipse(CheckedRect)
      end;
    end
    else
    begin
      if MenuItem.Checked then
        if not HasImgLstBitmap then
        begin
          if Enabled then
          begin
            Canvas.Pen.Color := CheckedColor;
            if Selected then
              Canvas.Brush.Color := CheckedAreaSelectColor
            else
              Canvas.Brush.Color := CheckedAreaColor;
          end
          else
            Canvas.Pen.Color := DisabledColor;

          Canvas.Brush.Style := bsSolid;
          Canvas.Rectangle(CheckedRect);
          if Enabled then
            Canvas.Pen.Color := clBlack
          else
            Canvas.Pen.Color := DisabledColor;
          x1 := CheckedRect.Left + 1;
          x2 := CheckedRect.Top + 5;
          Canvas.MoveTo(x1, x2);

          x1 := CheckedRect.Left + 4;
          x2 := CheckedRect.Bottom - 2;
          Canvas.LineTo(x1, x2);
          //--
          x1 := CheckedRect.Left + 2;
          x2 := CheckedRect.Top + 5;
          Canvas.MoveTo(x1, x2);

          x1 := CheckedRect.Left + 4;
          x2 := CheckedRect.Bottom - 3;
          Canvas.LineTo(x1, x2);
          //--
          x1 := CheckedRect.Left + 2;
          x2 := CheckedRect.Top + 4;
          Canvas.MoveTo(x1, x2);

          x1 := CheckedRect.Left + 5;
          x2 := CheckedRect.Bottom - 3;
          Canvas.LineTo(x1, x2);
          //-----------------

          x1 := CheckedRect.Left + 4;
          x2 := CheckedRect.Bottom - 3;
          Canvas.MoveTo(x1, x2);

          x1 := CheckedRect.Right + 2;
          x2 := CheckedRect.Top - 1;
          Canvas.LineTo(x1, x2);
          //--
          x1 := CheckedRect.Left + 4;
          x2 := CheckedRect.Bottom - 2;
          Canvas.MoveTo(x1, x2);

          x1 := CheckedRect.Right - 2;
          x2 := CheckedRect.Top + 3;
          Canvas.LineTo(x1, x2);
        end
        else
        begin
          if Enabled then
          begin
            Canvas.Pen.Color := SelectBorderColor;
            if Selected then
              Canvas.Brush.Color := CheckedAreaSelectColor
            else
              Canvas.Brush.Color := CheckedAreaColor;
          end
          else
            ACanvas.Pen.Color := DisabledColor;

          ACanvas.Brush.Style := bsSolid;
          ACanvas.Rectangle(CheckedRect);
        end;
    end;

  end;

  procedure DrawTheText(Sender: TObject; Text, ShortCutText: WideString;
    Canvas: TCanvas; TextRect: TRect;
    Selected, Enabled, Default: Boolean;
    var TextFont: TFont; TextFormat: Integer);
  var
    DefColor: TColor;
    R: TRect;
  begin
    R := TextRect;
    DefColor := TextFont.Color;
    Canvas.Font.Assign(TextFont);
    if Selected then DefColor := SelectFontColor;

    if not Enabled then
      DefColor := DisabledColor;

    Canvas.Font.Color := DefColor;    // will not affect Buttons
    if Default and Enabled then
    begin
      Inc(TextRect.Left, 1);
      Canvas.Font.Color := GetShadeColor(Canvas.Pixels[TextRect.Left, TextRect.Top], 30);
      ClxDrawTextW(Canvas, Text, TextRect, TextFormat);
      Dec(TextRect.Left, 1);

      Inc(TextRect.Top, 2);
      Inc(TextRect.Left, 1);
      Inc(TextRect.Right, 1);


      Canvas.Font.Color := GetShadeColor(Canvas.Pixels[TextRect.Left, TextRect.Top], 30);
      ClxDrawTextW(Canvas, Text, TextRect, TextFormat);

      Dec(TextRect.Top, 1);
      Dec(TextRect.Left, 1);
      Dec(TextRect.Right, 1);

      Canvas.Font.Color := GetShadeColor(Canvas.Pixels[TextRect.Left, TextRect.Top], 40);
      ClxDrawTextW(Canvas, Text, TextRect, TextFormat);

      Inc(TextRect.Left, 1);
      Inc(TextRect.Right, 1);

      Canvas.Font.Color := GetShadeColor(Canvas.Pixels[TextRect.Left, TextRect.Top], 60);
      ClxDrawTextW(Canvas, Text, TextRect, TextFormat);

      Dec(TextRect.Left, 1);
      Dec(TextRect.Right, 1);
      Dec(TextRect.Top, 1);

      Canvas.Font.Color := DefColor;
    end;

    ClxDrawTextW(Canvas, Text, TextRect, TextFormat);
    Text := ShortCutText + ' ';
    Canvas.Font.Color := GetShadeColor(DefColor, -40);

    Dec(TextRect.Right, 10);
    TextFormat := DT_RIGHT;

    ClxDrawTextW(Canvas, Text, TextRect, TextFormat);

    if TQMenuItem(Sender).Count > 0 then
    begin
      Canvas.Font.Name := 'Webdings';
      Text := '4';
      Dec(TextRect.Top, 3);
      TextRect.Right := R.Right;
      ClxDrawTextW(Canvas, Text, TextRect, TextFormat);
    end;
  end;

  procedure DrawIcon(Sender: TObject; Canvas: TCanvas; Bmp: TBitmap;
    IconRect: TRect; Hot, Selected, Enabled, Checked: Boolean);
  var
    DefColor: TColor;
    X, Y: integer;
  begin
    if (Bmp <> nil) and (Bmp.Width > 0) then
    begin
      X := IconRect.Left;
      Y := IconRect.Top + 1;
      if Sender is TQMenuItem then
      begin
        Inc(Y, 2);
        if IconWidth > Bmp.Width then
          X := X + ((IconWidth - Bmp.Width) div 2) - 1
        else
          X := IconRect.Left + 2;
      end;

      if Hot and (Enabled) and (not Checked) then
        if not Selected then
        begin
          Dec(X, 1);
          Dec(Y, 1);
        end;

      if (not Hot) and (Enabled) and (not Checked) then
        DimBitmap(Bmp, DimLevel{30});

      if not Enabled then
      begin
        GrayBitmap(Bmp, GrayLevel);
        DimBitmap(Bmp, 40);
      end;

      if (Hot) and (Enabled) and (not Checked) then
      begin
        DefColor := GetShadeColor(SelectColor, 50);
        DrawBitmapShadow(Bmp, Canvas, X + 2, Y + 2, DefColor);
      end;

      Bmp.Transparent := True;
      Canvas.Draw(X, Y, Bmp);
    end;
  end;

var
  Text: WideString;
  Bmp: TBitmap;
  IconRect, TextRect, CheckedRect: TRect;
  X1, X2: Integer;
  TextFormat: Integer;
  HasImgLstBitmap, HasBitmap: Boolean;
  IsLine: Boolean;
  Images: TCustomImageList;
  ImgIndex: Integer;
  ARect: TRect;
  Font: TFont;
  MenuItem: TQMenuItem;
  ClxMenuItem: TMenuItem;
  ps: Integer;
begin
  if Stage <> DrawStage_Pre then
  begin
    Stage := DrawStage_DefaultDraw;
    Exit;
  end;

  if not FMenusEnabled then Exit;

  ARect := Rect;
  MenuItem := TQMenuItem.Create;
  try
    ClxMenuItem := TMenuItem(FindObject(QObjectH(Item)));
    if ClxMenuItem <> nil then
    begin
      MenuItem.Checked := ClxMenuItem.Checked;
      MenuItem.RadioItem := ClxMenuItem.RadioItem;
      MenuItem.Enabled := ClxMenuItem.Enabled;
      MenuItem.Count := ClxMenuItem.Count;
      MenuItem.Caption := ClxMenuItem.Caption;
      MenuItem.ShortCut := ShortCutToText(ClxMenuItem.ShortCut);
    end
    else if Item <> nil then
    begin
      MenuItem.Checked := QMenuItem_isChecked(Item);
      QMenuItem_text(Item, @MenuItem.Caption);
      MenuItem.Enabled := Enabled;
      if QMenuItem_popup(Item) <> nil then
        MenuItem.Count := 1;
      if QMenuItem_isSeparator(Item) then MenuItem.Caption := '-';
      ps := Pos(#9, MenuItem.Caption);
      if ps > 0 then
      begin
        try
          MenuItem.ShortCut := Trim(Copy(MenuItem.Caption, ps + 1, MaxInt));
          MenuItem.Caption := Copy(MenuItem.Caption, 1, ps - 1);
        except
        end;
      end;
    end;
    IsLine := MenuItem.Caption = '-';

    if ClxMenuItem <> nil then
     // last menu item
      if ClxMenuItem.Parent.IndexOf(ClxMenuItem) = ClxMenuItem.Parent.Count - 1 then
        Dec(ARect.Bottom, 2);

    Inc(ARect.Bottom, 1);
    Dec(ARect.Right, 1);
    TextRect := ARect;
    Text := ' ' + MenuItem.Caption;

    Font := TFont.Create;
    Bmp := TBitmap.Create;
    try
      Canvas.Font.Assign(Font);
      HasBitmap := False;
      HasImgLstBitmap := False;

      if ClxMenuItem <> nil then
      begin
        if (ClxMenuItem.Parent.GetParentMenu.Images <> nil) then
          HasImgLstBitmap := ClxMenuItem.ImageIndex <> -1;

        if ClxMenuItem.Bitmap.Width > 0 then
          HasBitmap := True;

        if HasBitmap then
        begin
          Bmp.Width := ClxMenuItem.Bitmap.Width;
          Bmp.Height := ClxMenuItem.Bitmap.Height;
          Bmp.Canvas.CopyRect(Types.Rect(0, 0, Bmp.Width, Bmp.Height),
                              ClxMenuItem.Bitmap.Canvas,
                              Types.Rect(0, 0, Bmp.Width, Bmp.Height));
        end;
        if HasImgLstBitmap then
        begin
          if ClxMenuItem.Parent.GetParentMenu.Images <> nil then
          begin
            Images := ClxMenuItem.Parent.GetParentMenu.Images;
            ImgIndex := ClxMenuItem.ImageIndex;

            Bmp.Width := ClxMenuItem.Parent.GetParentMenu.Images.Width;
            Bmp.Height := ClxMenuItem.Parent.GetParentMenu.Images.Height;
            Bmp.Canvas.Brush.Color := Canvas.Pixels[2,2];
            Bmp.Canvas.FillRect(Types.Rect(0, 0, Bmp.Width, Bmp.Height));
            Images.Draw(Bmp.Canvas, 0, 0, ImgIndex);
          end;
        end;
      end;

      X1 := ARect.Left;
      X2 := ARect.Left + IconWidth;
      IconRect := Types.Rect(X1, ARect.Top, X2, ARect.Bottom);

      if HasImgLstBitmap or HasBitmap then
      begin
        CheckedRect := IconRect;
        Inc(CheckedRect.Left, 1);
        Inc(CheckedRect.Top, 2);
        Dec(CheckedRect.Right, 3);
        Dec(CheckedRect.Bottom, 2);
      end
      else
      begin
        CheckedRect.Left := IconRect.Left + (IConRect.Right - IconRect.Left - 10) div 2;
        CheckedRect.Top := IconRect.Top + (IConRect.Bottom - IconRect.Top - 10) div 2;
        CheckedRect.Right := CheckedRect.Left + 10;
        CheckedRect.Bottom := CheckedRect.Top + 10;
      end;

      if Bmp.Width > IconWidth then
        CheckedRect.Right := CheckedRect.Left + Bmp.Width;
      X1 := ARect.Left;
      Inc(X1, IconWidth);
      if (ARect.Left + Bmp.Width) > X1 then X1 := ARect.Left + Bmp.Width + 4;
      X2 := ARect.Right;

      TextRect := Types.Rect(X1, ARect.Top, X2, ARect.Bottom);

      Canvas.Brush.Color := Color;
      Canvas.FillRect(ARect);

      Canvas.Brush.Color := IconBackColor;
      Canvas.FillRect(IconRect);

      if Item = nil then Exit; // empty menu item

      if MenuItem.Enabled then
        Canvas.Font.Color := Font.Color
      else
        Canvas.Font.Color := DisabledColor;

      if Highlighted and DrawSelect then
      begin
        Canvas.Brush.Style := bsSolid;
        if MenuItem.Enabled then
        begin
          Inc(ARect.Top, 1);
          Dec(ARect.Bottom, 1);
          if FlatMenu then Dec(ARect.Right, 1);
          Canvas.Brush.Color := SelectColor;
          Canvas.FillRect(ARect);
          Canvas.Pen.Color := SelectBorderColor;
          Canvas.Brush.Style := bsClear;
          Canvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, 0, 0);
          Dec(ARect.Top, 1);
          Inc(ARect.Bottom, 1);
          if FlatMenu then Inc(ARect.Right, 1);
        end;
      end;

      DrawCheckedItem(MenuItem, Highlighted, MenuItem.Enabled,
        HasImgLstBitmap or HasBitmap, Canvas, CheckedRect);

      if not IsLine then
      begin
        TextFormat := 0;
        Inc(TextRect.Left, 3);
        TextRect.Top := TextRect.Top + ((TextRect.Bottom - TextRect.Top) - Canvas.TextHeight('W')) div 2;
        DrawTheText(MenuItem, Text, MenuItem.ShortCut, Canvas,
          TextRect, Highlighted, MenuItem.Enabled, False{MenuItem.Default},
          Font, TextFormat);
      end
      else
      begin
        X1 := TextRect.Left + 7;
        X2 := TextRect.Right;

        Canvas.Pen.Color := SeparatorColor;
        Canvas.MoveTo(X1, TextRect.Top + Round((TextRect.Bottom - TextRect.Top) / 2));
        Canvas.LineTo(X2, TextRect.Top + Round((TextRect.Bottom - TextRect.Top) / 2));
      end;

      DrawIcon(MenuItem, Canvas, Bmp, IconRect, Highlighted, False,
        MenuItem.Enabled, MenuItem.Checked);
    finally
      Bmp.Free;
      Font.Free;
    end;
  finally
    MenuItem.Free;
  end;
end;

procedure TThemedStyle.InternalDrawTab(Index: Integer);
const
  SELECTED_TAB_SIZE_DELTA = 2;
var
  TabControl: TOpenCustomTabControl;
  Details: TThemedElementDetails;
  Tab: TThemedTab;
  R: TRect;
  X, Y: Integer;

  function GetMaxTabHeight: Integer;
  var I: Integer;
  begin
    with TabControl do
    begin
      Result := 0;
      for I := 0 to Tabs.Count - 1 do
        if Tabs[I].Visible then
          if Result < Tabs[I].TabRect.Bottom then
            Result := Tabs[I].TabRect.Bottom;
    end;
  end;

  function RightSide: Integer;
  begin
    with TPrivateCustomTabControl(Self) do
    begin
      if FButtons[tbLeft].Visible then
        Result := FButtons[tbLeft].Left - 1
      else
        Result := Width - 2;
    end;
  end;

var
  OwnClipRect: Boolean;
  bMouseOver: Integer;
  ButtonRect: TRect;
begin
  TabControl := TOpenCustomTabControl(Self);
  with TabControl do
  begin
    if (Style = tsNoTabs) then
      Exit;

    Canvas.Start;
    OwnClipRect := False;
    if Canvas.ClipRect.Right <> RightSide then
    begin
      R := Rect(0, 0, RightSide, GetMaxTabHeight + 1);
      if not MultiLine then
      begin
        Canvas.SetClipRect(R);
        OwnClipRect := True;
      end;
    end;
    try
      R := Tabs[Index].TabRect;
      AdjustTabRect(R);
      ButtonRect := R;
      if Index = TabIndex then
        InflateRect(R, SELECTED_TAB_SIZE_DELTA, SELECTED_TAB_SIZE_DELTA);
      if R.Right > RightSide then R.Right := RightSide;

      if DrawTab(Index, R, Tabs[Index].Enabled) then
      begin
        case Style of
          tsTabs:
            begin
              Tab := ttTabItemNormal;
              if Index = TabIndex then Tab := ttTabItemSelected;
              if Tabs[Index].Highlighted then Tab := ttTabItemLeftEdgeFocused;
              if TPrivateCustomTabControl(TabControl).FMouseOver = Index then
                {if IsMouseOver(QWidgetH(TabControl.Handle), @R) then }Tab := ttTabItemHot;
              if not Tabs[Index].Enabled then Tab := ttTabItemDisabled;

              Details := ThemeServices.GetElementDetails(Tab);
              ThemeServices.DrawElement(Canvas, Details, R);

              R := Tabs[Index].TabRect;
              InflateRect(R, -3, -3);
              Inc(R.Bottom, 3);
              if Index <> TabIndex then Inc(R.Top);
              {$IFDEF LINUX}
              Dec(R.Top, 2);
              {$ENDIF LINUX}
              X := R.Left;
              Y := R.Top;
            end;

          tsButtons, tsFlatButtons:
            begin
              if Style = tsFlatButtons then
              begin
                if Index > TPrivateCustomTabControl(TabControl).FFirstVisibleTab then
                begin
                  Canvas.Pen.Style := psSolid;
                  Canvas.Pen.Color := clGray;
                  Canvas.MoveTo(ButtonRect.Left - 4, ButtonRect.Top + 2);
                  Canvas.LineTo(ButtonRect.Left - 4, ButtonRect.Bottom - 2);
                  Canvas.Pen.Color := clWhite;
                  Canvas.MoveTo(ButtonRect.Left - 3, ButtonRect.Top + 3);
                  Canvas.LineTo(ButtonRect.Left - 3, ButtonRect.Bottom - 1);
                end;
              end;

              if HotTrack and (TPrivateCustomTabControl(TabControl).FMouseOver = Index) then
                bMouseOver := 1
              else
                bMouseOver := 2;
              if (Style <> tsFlatButtons) or (Index = TabIndex) or (bMouseOver <> 2) then
                DrawButtonFaceEx(Canvas, ButtonRect, 1, Index = TabIndex, Index = TabIndex,
                  Style = tsFlatButtons, bMouseOver);
              with Tabs[Index].TabRect do
              begin
                X := Left + ((Right - Left) - Canvas.TextWidth(Tabs[Index].Caption)) div 2;
                Y := Top + ((Bottom - Top) - Canvas.TextHeight(Tabs[Index].Caption)) div 2 + 1;
              end;
            end;

          else
            Exit;
        end; // case

        if (Tabs[Index].ImageIndex <> -1) and (Images <> nil) then
        begin
          if Style <> tsTabs then
            Dec(X, (Images.Width + 1) div 2);
          Images.Draw(Canvas, X, Y, Tabs[Index].ImageIndex, itImage, Tabs[Index].Enabled);
          Inc(X, Images.Width + 2);
        end;
        if (Style <> tsTabs) and (Index = TabIndex) then
        begin
          Inc(Y);
          Inc(X);
        end;
        Canvas.TextRect(R, X, Y, Tabs[Index].Caption,
          Integer(AlignmentFlags_SingleLine) or Integer(AlignmentFlags_ShowPrefix));

        if (Index = TabIndex) and Focused and (Style = tsTabs) then
        begin
          R := Tabs[Index].TabRect;
          InflateRect(R, -1, -1);
          Inc(R.Top);
          Canvas.DrawFocusRect(R);
        end;
      end;
    finally
      if OwnClipRect then
        Canvas.ResetClipRegion;
      Canvas.Stop;
    end;
  end;
end;

procedure TThemedStyle.HookedCustomTabControlPaint;
// Self = TCustomTabControl
var
  Details: TThemedElementDetails;
  TabControl: TOpenCustomTabControl;
  R: TRect;
  I: Integer;

  function GetMaxTabHeight: Integer;
  var I: Integer;
  begin
    with TabControl do
    begin
      Result := 0;
      for I := 0 to Tabs.Count - 1 do
        if Tabs[I].Visible then
          if Result < Tabs[I].TabRect.Bottom then
            Result := Tabs[I].TabRect.Bottom;
    end;
  end;

  function RightSide: Integer;
  begin
    with TPrivateCustomTabControl(Self) do
    begin
      if FButtons[tbLeft].Visible then
        Result := FButtons[tbLeft].Left - 1
      else
        Result := Width - 2;
    end;
  end;

  function InheritedCall: Boolean;
  var
    OldProc: procedure(Self: TObject);
  begin
    with TOpenCustomTabControl(Self) do
    begin
      Canvas.Start;
      QPainter_save(Canvas.Handle);
      try
        QIntersectClipRect(Canvas.Handle, Rect(-1, -1, -1, -1));
        CodeRestore(@TOpenCustomTabControl.Paint, ThemedStyle.FCodeDrawTab);
        try
          OldProc := @TOpenCustomTabControl.Paint;
          OldProc(Self); // enable buttons, ...
        finally
          CodeRedirect(@TOpenCustomTabControl.Paint, @TThemedStyle.HookedCustomTabControlPaint, ThemedStyle.FCodeDrawTab);
        end;
      finally
        QPainter_restore(Canvas.Handle);
        Canvas.Stop;
      end;
      Result := Style = tsNoTabs;
    end; // with
  end;

begin
  TabControl := TOpenCustomTabControl(Self);
  with TabControl do
  begin
    if (TPrivateCustomTabControl(TabControl).FLayoutCount > 0) then
      LayoutTabs;
    if (not MultiLine) or (Style = tsNoTabs) then
      if InheritedCall then Exit;

    Canvas.Start;
    try
      R := ClientRect;
      ThemeServices.DrawParentBackground(Handle, Canvas, nil, False);

      R.Top := GetMaxTabHeight;
      Details := ThemeServices.GetElementDetails(ttPane);
      ThemeServices.DrawElement(Canvas, Details, R);

      R := Rect(0, 0, RightSide, GetMaxTabHeight + 1);
      if not MultiLine then
        Canvas.SetClipRect(R);
      try
        with TPrivateCustomTabControl(TabControl) do
        begin
          if Tabs.Count > 0 then
          begin
            for I := FFirstVisibleTab to FLastVisibleTab do
            begin
              if (Tabs[I].Visible) and (I <> TabIndex) then
                InternalDrawTab(I);
            end;
            if (TabIndex <> -1) and (Tabs[TabIndex].Visible) then
              if (TabIndex >= FFirstVisibleTab) and (TabIndex <= FLastVisibleTab) then
                InternalDrawTab(TabIndex);
          end;
        end;
      finally
        if not MultiLine then
          Canvas.ResetClipRegion;
      end;
    finally
      Canvas.Stop;
    end;
  end;
end;

procedure TThemedStyle.HookedDoHotTrack(Index: Integer);
  procedure DrawHotTrack(Erase: Boolean);
  var
    idx: Integer;
    i: Integer;
  begin
    with TPrivateCustomTabControl(Self), TOpenCustomTabControl(Self) do
      if FMouseOver <> -1 then
      begin
        idx := FMouseOver;
        if Erase then
          FMouseOver := -1;
        InternalDrawTab(idx);
        if Erase and (Style = tsFlatButtons) then
          for i := FFirstVisibleTab to FLastVisibleTab do
            InvalidateRect(Tabs[i].TabRect, False);
      end;
  end;
begin
  with TPrivateCustomTabControl(Self), TOpenCustomTabControl(Self) do
  begin
    if (FMouseOver <> Index) then
    begin
      if MultiLine or ((FMouseOver >= FFirstVisibleTab) and
         (FMouseOver <= FLastVisibleTab)) then
        DrawHotTrack(True);
      FMouseOver := Index;
      if HotTrack or (FTracking = Index) then
        DrawHotTrack(False);
    end;
  end;
end;

procedure TThemedStyle.HookedTabSheetPaint;
// Self = TTabSheet
var
  Details: TThemedElementDetails;
  TabSheet: TTabSheet;
begin
  TabSheet := TTabSheet(Self);
  if TabSheet.PageControl.Style = tsTabs then
  begin
    Details := ThemeServices.GetElementDetails(ttBody);
    ThemeServices.DrawElement(TOpenCustomControl(TabSheet).Canvas, Details, TabSheet.ClientRect);
  end;
end;

procedure TThemedStyle.HookedProgressBarPaint;
// Self = TProgressBar
var
  Details: TThemedElementDetails;
  Progress: TThemedProgress;
  ProgressBar: TOpenProgressBar;
  R: TRect;
begin
  ProgressBar := TOpenProgressBar(Self);
  with ProgressBar do
  begin
    Transparent := True; // prevent that SetPosition() calls InternalPaint
    if Orientation = pbHorizontal then Progress := tpBar else Progress := tpBarVert;

    R := ClientRect;
   // fix WindowsXP theming bug with Height < 10
    if (Orientation = pbHorizontal) and (Height < 10) then
      Dec(R.Top, 10 - Height)
    else if (Orientation = pbVertical) and (Width < 10) then
      Dec(R.Left, 10 - Width);

    Details := ThemeServices.GetElementDetails(Progress);
    ThemeServices.DrawElement(Canvas, Details, R);
    R := ThemeServices.ContentRect(Canvas, Details, ClientRect);

    if Position > Min then
    begin
      if Orientation = pbHorizontal then
      begin
        Progress := tpChunk;
        R.Right := MulDiv(Abs(Position - Min), (R.Right - R.Left), (Max - Min));
      end
      else
      begin
        Progress := tpChunkVert;
        R.Top := MulDiv(Abs(Position - Max), (R.Bottom - R.Top), (Max - Min));
      end;
      Details := ThemeServices.GetElementDetails(Progress);
      ThemeServices.DrawElement(Canvas, Details, R, @R);
    end;
  end;
end;

procedure TThemedStyle.HookedWidgetControlPainting(Sender: QObjectH; EventRegion: QRegionH);
var
  ForcedPaintEvent: QPaintEventH;
  Canvas: TControlCanvas;
begin
  with TOpenWidgetControl(Self) do
  begin
    if TWidgetControl(Self) is TCustomGroupBox then
    begin
      Canvas := TControlCanvas.Create;
      try
        Canvas.Control := TWidgetControl(Self);
        Canvas.StartPaint;
        try
          QPainter_setClipRegion(Canvas.Handle, EventRegion);
          ThemedStyle.DrawGroupBox(TCustomGroupBox(Self), Canvas);
        finally
          Canvas.StopPaint;
        end;
      finally
        Canvas.Free;
      end;
      Exit;
    end;

   // original code 
    ForcedPaintEvent := QPaintEvent_create(EventRegion, False);
    try
      ControlState := ControlState + [csWidgetPainting];
      try
        QObject_event(Sender, ForcedPaintEvent);
      finally
        ControlState := ControlState - [csWidgetPainting];
      end;
    finally
      QPaintEvent_destroy(ForcedPaintEvent);
    end;
  end;
end;

procedure TThemedStyle.DrawGroupBox(Control: TCustomGroupBox; Canvas: TCanvas);
var
  Details: TThemedElementDetails;
  R, CaptionRect: TRect;
begin
  with TOpenCustomGroupBox(Control) do
  begin
    R := BoundsRect;
    OffsetRect(R, -R.Left, -R.Top);
    ThemeServices.DrawParentBackground(Handle, Canvas, nil, False, @R);

    if Enabled then
      Details := ThemeServices.GetElementDetails(tbGroupBoxNormal)
    else
      Details := ThemeServices.GetElementDetails(tbGroupBoxDisabled);

    Inc(R.Top, Canvas.TextHeight('0') div 2);
    ThemeServices.DrawElement(Canvas.Handle, Details, R);

    CaptionRect := Rect(9, 0,
      Min(Canvas.TextWidth(Caption) + 9, ClientWidth - 8), Canvas.TextHeight(Caption));

    ThemeServices.DrawParentBackground(Handle, Canvas, nil, False, @CaptionRect);
    ThemeServices.DrawText(Canvas, Details, Caption, CaptionRect, DT_LEFT, 0);
  end;
end;

procedure TThemedStyle.HookedToolButtonPaint;
// Self = TToolButton

const
  TBSpacing = 3;

  procedure DrawDropDown;
  var
    R: TRect;
    MidX,
    MidY: Integer;
    Pts: array of TPoint;
  begin
    with TOpenToolButton(Self) do
    begin
      R := Types.Rect(Width - DropDownWidth, 1, Width, Height - 1);
      if Down then
        OffsetRect(R, 1, 1);
      Canvas.Pen.Color := clButtonText;
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := clButtonText;
      MidX := R.Left + (R.Right - R.Left) div 2;
      MidY := R.Top + (R.Bottom - R.Top) div 2;
      SetLength(Pts, 4);
      Pts[0] := Types.Point(MidX - 2, MidY - 1);
      Pts[1] := Types.Point(MidX + 2, MidY - 1);
      Pts[2] := Types.Point(MidX, MidY + 1);
      Pts[3] := Pts[0];
      Canvas.Polygon(Pts);
      if not FToolBar.Flat or (FToolBar.Flat and IsMouseOver(TControl(Self))) then
        DrawEdge(Canvas, R, esRaised, esLowered, [ebLeft]);
    end;
  end;

  function CaptionRect(var R: TRect): TRect;
  begin
    with TOpenToolButton(Self) do
    begin
      if Style = tbsDropDown then
        Dec(R.Right, 14);
      Result := R;
      if (ToolBar.Images <> nil) and (ToolBar.Images.Count > 0) then
      begin
        if ToolBar.List then
        begin
          R.Left := R.Left + TBSpacing;
          R.Right := R.Left + ToolBar.Images.Width;
          R.Top := ((R.Bottom - R.Top) div 2) - (ToolBar.Images.Height div 2);
          R.Bottom := R.Top + ToolBar.Images.Height;
          Result.Left := R.Right;
        end
        else begin
          R.Left := ((R.Right - R.Left) div 2) - (ToolBar.Images.Width div 2);
          R.Top := R.Top + TBSpacing;
          R.Right := R.Left + ToolBar.Images.Width;
          R.Bottom := R.Top + ToolBar.Images.Height;
        end;
      end;
      if ToolBar.List then
        Result.Top := Result.Top + TBSpacing
      else if (ToolBar.Images <> nil) and (ToolBar.Images.Count > 0) then
        Result.Top := R.Bottom + TBSpacing
      else
        Result.Top := R.Top + TBSpacing;
      Result.Left := Result.Left + TBSpacing;
      Result.Right := Result.Right - TBSpacing;
      Result.Bottom := Result.Bottom - TBSpacing;
    end;
  end;

var
  Details: TThemedElementDetails;
  Bar: TThemedToolBar;
  R, CR: TRect;
  Ind: Boolean;
  DrawFlags: Integer;
begin
  with TOpenToolButton(Self) do
  begin
    if FToolBar = nil then Exit;

    case Style of
      tbsButton, tbsCheck:
        begin
          if ToolBar.Flat then Bar := ttbButtonNormal else Bar := ttbButtonHot;
          if IsMouseOver(TControl(Self)) then
            if Marked then Bar := ttbButtonCheckedHot else Bar := ttbButtonHot;
          if Down then Bar := ttbButtonPressed;
          if Indeterminate or Marked then Bar := ttbButtonChecked;
          if not Enabled then Bar := ttbButtonDisabled;
        end;
      tbsDropDown:
        begin
          if ToolBar.Flat then Bar := ttbDropDownButtonNormal else Bar := ttbDropDownButtonHot;
          if IsMouseOver(TControl(Self)) then
            if Marked then Bar := ttbDropDownButtonCheckedHot else Bar := ttbDropDownButtonHot;
          if Down then Bar := ttbDropDownButtonPressed;
          if Indeterminate or Marked then Bar := ttbDropDownButtonChecked;
          if not Enabled then Bar := ttbDropDownButtonDisabled;
        end;
      tbsSeparator, tbsDivider:
        begin
          if Toolbar.Align in [alLeft, alRight] then
          begin
            Bar := ttbSeparatorVertNormal;
            if IsMouseOver(TControl(Self)) then
              if Marked then Bar := ttbSeparatorVertCheckedHot else Bar := ttbSeparatorVertHot;
            if Down then Bar := ttbSeparatorVertPressed;
            if Indeterminate or Marked then Bar := ttbSeparatorVertChecked;
            if not Enabled then Bar := ttbSeparatorVertDisabled;
          end
          else
          begin
            Bar := ttbSeparatorNormal;
            if IsMouseOver(TControl(Self)) then
              if Marked then Bar := ttbSeparatorCheckedHot else Bar := ttbSeparatorHot;
            if Down then Bar := ttbSeparatorPressed;
            if Indeterminate or Marked then Bar := ttbSeparatorChecked;
            if not Enabled then Bar := ttbSeparatorDisabled;
          end;
        end;
    else
      Exit;
    end;

    R := ClientRect; 
    ThemeServices.DrawParentBackground(Handle, Canvas, nil, False, @R);

    Details := ThemeServices.GetElementDetails(Bar);
    ThemeServices.DrawElement(Canvas, Details, ClientRect, nil);

  // draw content
    if Style in [tbsDropDown, tbsButton, tbsCheck] then
    begin
      DrawFlags := Integer(AlignmentFlags_ShowPrefix) or Integer(AlignmentFlags_AlignVCenter);
      if FToolBar.List then
        DrawFlags := DrawFlags or Integer(AlignmentFlags_AlignLeft)
      else
        DrawFlags := DrawFlags or Integer(AlignmentFlags_AlignHCenter);

      if Style = tbsDropDown then DrawDropDown;
      Ind := Indeterminate and not Down;

      R := ClientRect;
      CR := CaptionRect(R);
      if Down then
      begin
        OffsetRect(R, 1, 1);
        OffsetRect(CR, 1, 1);
      end;

      //draw image
      Canvas.Brush.Style := bsSolid;
      if (ImageIndex > -1) and (ToolBar.Images <> nil) and (ToolBar.Images.Count > 0) then
      begin
        if Assigned(ToolBar.HotImages) and IsMouseOver(TControl(Self)) and IsEnabled
        and (ImageIndex < ToolBar.HotImages.Count) then
          Toolbar.HotImages.Draw(Canvas, R.Left, R.Top, ImageIndex, itImage, not Ind)
        else if Assigned(ToolBar.DisabledImages) and not IsEnabled and
          (ImageIndex < ToolBar.DisabledImages.Count) then
          Toolbar.DisabledImages.Draw(Canvas, R.Left, R.Top, ImageIndex,
            itImage, not Ind)
        else if Assigned(ToolBar.Images) and
          (ImageIndex < ToolBar.Images.Count) then
          ToolBar.Images.Draw(Canvas, R.Left, R.Top, ImageIndex, itImage,
            IsEnabled and not Ind);
      end;

      { draw caption }
      if (Caption <> '') and ToolBar.ShowCaptions then
      begin
        Canvas.Brush.Style := bsClear;
        Canvas.Font := ToolBar.Font;
        if not IsEnabled then
          Canvas.Font.Color := clDisabledText;
        Canvas.TextRect(CR, CR.Left, CR.Top, Caption, DrawFlags);
      end;
    end;
  end;
end;

{
procedure TThemedStyle.HookedToolBarPaint;
// Self = TToolBar
var
  Details: TThemedElementDetails;
begin
  with TOpenToolBar(Self) do
  begin
    Details := ThemeServices.GetElementDetails(trRebarRoot);
    ThemeServices.DrawElement(Canvas, Details, ClientRect);
  end;
end;
}

procedure TThemedStyle.HookedSpeedButtonPaint;
// Self = TSpeedButton
var
  P: procedure(Self: TObject);
begin
  with TOpenSpeedButton(Self) do
  begin
   // call original method
    P := @TOpenSpeedButton.Paint;
    CodeRestore(@TOpenSpeedButton.Paint, ThemedStyle.FCodeSpeedButtonPaint);
    try
      P(Self);
    finally
      CodeRedirect(@TOpenSpeedButton.Paint, @TThemedStyle.HookedSpeedButtonPaint, ThemedStyle.FCodeSpeedButtonPaint);
    end;

    if Flat then
    begin
     // redraw button border, no button content
      if Down or (FState = bsExclusive) then
        DrawThemedFlatButtonFace(Canvas, ClientRect, True, False);
    end;
  end;
end;

procedure TThemedStyle.HookedControlMouseMove(Shift: TShiftState; X, Y: Integer);
// Self = TControl
var
  Index: Integer;
  NewSection: Integer;
  P: PInteger;
begin
  with TOpenControl(Self) do
  begin
    if TControl(Self) is TCustomHeaderControl then
    begin
      Index := ThemedStyle.FControlInfo.IndexOfObject(Self);
      if Index = -1 then
      begin
        Index := ThemedStyle.FControlInfo.AddObject('0', Self);
        TComponent(Self).FreeNotification(ThemedStyle.FNotifyObject);
      end;
      if TOpenCustomHeaderControl(Self).Orientation = hoHorizontal then
        P := @X
      else
        P := @Y;
      NewSection := QHeader_sectionAt(QHeaderH(THeaderControl(Self).Handle), P^);
      if NewSection + 1 <> StrToIntDef(ThemedStyle.FControlInfo[Index], 0) then
      begin
        ThemedStyle.FControlInfo[Index] := IntToStr(NewSection + 1);
        PaintControl(TControl(Self));
      end;
    end;

   // original code
    Application.HintMouseMessage(TControl(Self), Shift, X, Y);
    if Assigned(OnMouseMove) then
      OnMouseMove(TControl(Self), Shift, X, Y);
  end;
end;

function TThemedStyle.HookedListViewHeaderEventFilter(Sender: QObjectH; Event: QEventH): Boolean;
// Self = TListViewHeader
var
  Method: TMethod;
  Index: Integer;
begin
  with TPrivateListViewHeader(Self) do
  begin
    if FHidden then
    begin
      Method.Code := @TOpenCustomHeaderControl.EventFilter;
      Method.Data := Self;
      Result := TEventFilter(Method)(Sender, Event);
    end
    else
    begin
      case QEvent_type(Event) of
        QEventType_MouseMove:
          begin
            if QHeader_orientation(QHeaderH(Handle)) = Orientation_Vertical then
              Index := QHeader_sectionAt(QHeaderH(Handle), QMouseEvent_y(QMouseEventH(Event)))
            else
              Index := QHeader_sectionAt(QHeaderH(Handle), QMouseEvent_x(QMouseEventH(Event)));
            if Tag <> Index + 1 then
            begin
              Tag := Index + 1;
              PaintWidget(QWidgetH(Sender));
            end;
          end;
        QEventType_Leave:
          begin
            PaintWidget(QWidgetH(Sender));
            Tag := 0;
          end;
      end;
      Result := False;
    end;
  end;
end;

procedure TThemedStyle.Initialize;
type
  QClxStyle_drawPushButton_Event = procedure (btn: QPushButtonH; p: QPainterH; var Stage: Integer) of object cdecl;
  QClxStyle_drawPopupMenuItem_Event = procedure (p: QPainterH; checkable: Boolean; maxpmw: Integer; tab: Integer; mi: QMenuItemH; itemID: Integer; act: Boolean; enabled: Boolean; x: Integer; y: Integer; w: Integer; h: Integer; var Stage: Integer) of object cdecl;
var
  Method: TMethod;
  F: QFrameH;
  ViewControl: TOpenCustomViewControl;
  EventEvent: TEventFilter;
begin
  with Application do
  begin
   // Button
    Style.BeforeDrawButton := EvBeforeDrawButton;
    Style.AfterDrawButton := EvAfterDrawButton;

    Style.DrawComboButton := EvDrawComboButton;

    Style.DrawCheck := EvDrawCheck;
    Style.DrawRadio := EvDrawRadio;
    Style.DrawRadioMask := EvDrawRadioMask;
    Style.DrawFrame := EvDrawFrame;
    Style.DrawScrollBar := EvDrawScrollBar;

    Style.DrawTrackBar := EvDrawTrackBar;
    Style.DrawTrackBarMask := EvDrawTrackBarMask;
    Style.DrawTrackBarGroove := EvDrawTrackBarGroove;

    Style.DrawHeaderSection := EvDrawHeaderSection;

    Style.DrawMenuFrame := EvDrawMenuFrame;
    Style.MenuItemHeight := EvMenuItemHeight;
  end;

  QClxStyle_drawPushButton_Event(Method) := DrawPushButtonHook;
  QClxStyleHooks_hook_drawPushButton(Hooks, Method);

  QClxStyle_drawPopupMenuItem_Event(Method) := DrawPopupMenuItemHook;
  QClxStyleHooks_hook_drawPopupMenuItem(Hooks, Method);

  {$IFDEF LINUX}
  QClxStyle_drawToolButton_Event(Method) := DoDrawToolButtonHook;
  QClxStyleHooks_hook_drawToolButton(Hooks, Method);

  QClxStyle_drawToolButton2_Event(Method) := DoDrawToolButton2Hook;
  QClxStyleHooks_hook_drawToolButton2(Hooks, Method);
  {$ENDIF LINUX}

  CodeRedirect(@TOpenCustomTabControl.Paint, @TThemedStyle.HookedCustomTabControlPaint, ThemedStyle.FCodeDrawTab);
  ReplaceVmtField(TTabSheet, @TOpenCustomControl.Paint, @TThemedStyle.HookedTabSheetPaint);
  CodeRedirect(@TOpenProgressBar.Paint, @TThemedStyle.HookedProgressBarPaint, ThemedStyle.FCodeProgressBarPaint);
  CodeRedirect(@TOpenWidgetControl.Painting, @TThemedStyle.HookedWidgetControlPainting, ThemedStyle.FCodeWidgetControlPainting);
  CodeRedirect(@TOpenToolButton.Paint, @TThemedStyle.HookedToolButtonPaint, ThemedStyle.FCodeToolButtonPaint);
//  CodeRedirect(@TOpenToolBar.Paint, @TThemedStyle.HookedToolBarPaint, ThemedStyle.FCodeToolBarPaint);
  CodeRedirect(@TOpenSpeedButton.Paint, @TThemedStyle.HookedSpeedButtonPaint, ThemedStyle.FCodeSpeedButtonPaint);
  CodeRedirect(@TOpenControl.MouseMove, @TThemedStyle.HookedControlMouseMove, ThemedStyle.FCodeControlMouseMove);

  FDoHotTrack := GetSubCallAddress(@TOpenCustomTabControl.MouseLeave, 2);
  if FDoHotTrack <> nil then
    CodeRedirect(FDoHotTrack, @TThemedStyle.HookedDoHotTrack, ThemedStyle.FCodeCustomTabControlDoHotTrack);

  F := QFrame_create(nil, nil, 0, False);
  try
    OrgQFrame_drawFrame := GetAddress_QFrame_drawFrame(F);
  finally
    QFrame_destroy(F);
  end;
  CodeRedirect(@OrgQFrame_drawFrame, @HookedQFrame_drawFrame, ThemedStyle.FCodeQFrame_drawFrame);

  ViewControl := TOpenCustomViewControl.Create(nil);
  try
    EventEvent := TOpenCustomHeaderControl(ViewControl.Columns.Owner).EventFilter;
    FOrgListViewHeaderEventFilter := TMethod(EventEvent).Code;
  finally
    ViewControl.Free;
  end;

  CodeRedirect(FOrgListViewHeaderEventFilter, @TThemedStyle.HookedListViewHeaderEventFilter,
    FCodeListViewHeaderEventFilters);


  CodeRedirect(@DrawButtonFace, @DrawThemedButtonFace, ThemedStyle.FCodeDrawButtonFace);
  CodeRedirect(@DrawEdge, @DrawThemedEdge, ThemedStyle.FCodeDrawEdge);

  OrgqDrawShadePanel := GetProcAddress(GetModuleHandle(PChar(QtIntf)), QtNamePrefix + 'QClxDrawUtil_DrawShadePanel2');
  if Assigned(OrgqDrawShadePanel) then
    CodeRedirect(@OrgqDrawShadePanel, @DrawThemedShadePanel, ThemedStyle.FCodeDrawShadePanel);

  FOldAppIdle := TTimer(PrivateApp.FIdleTimer).OnTimer;
  TTimer(PrivateApp.FIdleTimer).OnTimer := AppIdle;
end;

procedure TThemedStyle.Finalize;
begin
  if Assigned(PrivateApp.FIdleTimer) then
    TTimer(PrivateApp.FIdleTimer).OnTimer := FOldAppIdle;

  CodeRestore(@TOpenCustomTabControl.Paint, ThemedStyle.FCodeDrawTab);
  ReplaceVmtField(TTabSheet, @TThemedStyle.HookedTabSheetPaint, @TOpenCustomControl.Paint);
  CodeRestore(@TOpenProgressBar.Paint, ThemedStyle.FCodeProgressBarPaint);
  CodeRestore(@TOpenWidgetControl.Painting, ThemedStyle.FCodeWidgetControlPainting);
  CodeRestore(@TOpenToolButton.Paint, ThemedStyle.FCodeToolButtonPaint);
//  CodeRestore(@TOpenToolBar.Paint, ThemedStyle.FCodeToolBarPaint);
  CodeRestore(@TOpenSpeedButton.Paint, ThemedStyle.FCodeSpeedButtonPaint);
  CodeRestore(@TOpenControl.MouseMove, ThemedStyle.FCodeControlMouseMove);
  CodeRestore(FOrgListViewHeaderEventFilter, FCodeListViewHeaderEventFilters);
  if FDoHotTrack <> nil then
    CodeRestore(FDoHotTrack, ThemedStyle.FCodeCustomTabControlDoHotTrack);


  CodeRestore(@DrawButtonFace, ThemedStyle.FCodeDrawButtonFace);
  CodeRestore(@DrawEdge, ThemedStyle.FCodeDrawEdge);
  CodeRestore(@OrgQFrame_drawFrame, ThemedStyle.FCodeQFrame_drawFrame);

  if Assigned(OrgqDrawShadePanel) then
    CodeRestore(@OrgqDrawShadePanel, ThemedStyle.FCodeDrawShadePanel);
end;

procedure TThemedStyle.AppIdle(Sender: TObject);
var
  Control: TControl;
  App: TPrivateApplication;
begin
  App := TPrivateApplication(Application);
  Control := App.FMouseControl;
  FOldAppIdle(Sender);
  if Control <> App.FMouseControl then
  begin
    if Control <> nil then
      MouseLeave(Control);
    if App.FMouseControl <> nil then
      MouseEnter(App.FMouseControl);
  end;
end;

procedure TThemedStyle.MouseEnter(Control: TControl);
begin
  if (Control is TButton) or
     (Control is TCustomHeaderControl) or
     (Control is TScrollBar) then
    RepaintControl(Control);

  if (Control is TSpeedButton) and not TSpeedButton(Control).Flat then
    TOpenSpeedButton(Control).Paint;
end;

procedure TThemedStyle.MouseLeave(Control: TControl);
begin
  if (Control is TButton) or
     (Control is TCustomHeaderControl) or
     (Control is TScrollBar) then
    RepaintControl(Control);

  if (Control is TSpeedButton) then
  begin
    if TSpeedButton(Control).Flat then
      Control.Invalidate
    else
      TOpenSpeedButton(Control).Paint;
  end
  else
  if Control is TCustomHeaderControl then
    FNotifyObject.Notification(Control, opRemove);
end;

function TThemedStyle.GetActive: Boolean;
begin
  Result := (Application.Style = Self) and
    (Application.Style.DefaultStyle = dsSystemDefault);
end;

procedure TThemedStyle.SetActive(const Value: Boolean);
begin
  SetDefaultStyle(dsSystemDefault);
end;

procedure TThemedStyle.SetDefaultStyle(const Value: TDefaultStyle);
begin
  if Value <> DefaultStyle then
  begin
    if Value = dsSystemDefault then
    begin
      Application.Style := nil; // destroys ThemedStyle
      TThemedStyle.Create; // assigns Application.Style to ThemedStyle
      Exit;
    end
    else
      if Active then
        Finalize;
    inherited SetDefaultStyle(Value);
  end;
end;

{ TThemedStyleNotify }

procedure TThemedStyleNotify.Notification(AComponent: TComponent;
  Operation: TOperation);
var Index: Integer;
begin
  inherited;
  if Operation = opRemove then
  begin
    Index := ThemedStyle.FControlInfo.IndexOfObject(AComponent);
    if Index >= 0 then
      ThemedStyle.FControlInfo.Delete(Index);
  end;
end;

initialization
  ThemedStyle := nil;
  ThemeServices.ThemesDir:=getSystemInfo(XP_BASE_DIR);
  if ThemeServices.ThemesEnabled then
    TThemedStyle.Create; // sets ThemedStyle

  application.font.name:='tahoma';
  application.font.Size:=11;

finalization
  if ThemedStyle <> nil then
    Application.Style := nil; // frees ThemedStyle

end.

