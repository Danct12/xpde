unit QThemeSrvLinux;
interface
uses
  Qt, Types, SysUtils,
  Classes, Contnrs, QGraphics,
  QControls, QForms,
  QImgList, QStdCtrls, QComCtrls;

type
  // These are all elements which can be themed.
  TThemedElement = (
    teButton,
    teClock,
    teComboBox,
    teEdit,
    teExplorerBar,
    teHeader,
    teListView,
    teMenu,
    tePage,
    teProgress,
    teRebar,
    teScrollBar,
    teSpin,
    teStartPanel,
    teStatus,
    teTab,
    teTaskBand,
    teTaskBar,
    teToolBar,
    teToolTip,
    teTrackBar,
    teTrayNotify,
    teTreeview,
    teWindow
  );

  // 'Button' theme data
  TThemedButton = (
    tbButtonDontCare,
    tbButtonRoot,       // The root part of each element is sometimes used for special painting and does not
                        // belong to a certain state.
    tbPushButtonNormal, tbPushButtonHot, tbPushButtonPressed, tbPushButtonDisabled, tbPushButtonDefaulted,
    tbRadioButtonUncheckedNormal, tbRadioButtonUncheckedHot, tbRadioButtonUncheckedPressed, tbRadioButtonUncheckedDisabled,
    tbRadioButtonCheckedNormal, tbRadioButtonCheckedHot, tbRadioButtonCheckedPressed, tbRadioButtonCheckedDisabled,
    tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedHot, tbCheckBoxUncheckedPressed, tbCheckBoxUncheckedDisabled,
    tbCheckBoxCheckedNormal, tbCheckBoxCheckedHot, tbCheckBoxCheckedPressed, tbCheckBoxCheckedDisabled,
    tbCheckBoxMixedNormal, tbCheckBoxMixedHot, tbCheckBoxMixedPressed, tbCheckBoxMixedDisabled,
    tbGroupBoxNormal, tbGroupBoxDisabled,
    tbUserButton
  );

  // 'Clock' theme data
  TThemedClock = (
    tcClockDontCare,
    tcClockRoot,
    tcTimeNormal
  );

  // 'ComboBox' theme data
  TThemedComboBox = (
    tcComboBoxDontCare,
    tcComboBoxRoot,
    tcDropDownButtonNormal, tcDropDownButtonHot,  tcDropDownButtonPressed,  tcDropDownButtonDisabled
  );

  // 'Edit' theme data
  TThemedEdit = (
    teEditDontCare,
    teEditRoot,
    teEditTextNormal, teEditTextHot, teEditTextSelected, teEditTextDisabled, teEditTextFocused, teEditTextReadOnly, teEditTextAssist,
    teEditCaret
  );

  // 'ExplorerBar' theme data
  TThemedExplorerBar = (
    tebExplorerBarDontCare,
    tebExplorerBarRoot,
    tebHeaderBackgroundNormal, tebHeaderBackgroundHot,  tebHeaderBackgroundPressed,
    tebHeaderCloseNormal, tebHeaderCloseHot, tebHeaderClosePressed,
    tebHeaderPinNormal, tebHeaderPinHot, tebHeaderPinPressed,
    tebHeaderPinSelectedNormal, tebHeaderPinSelectedHot, tebHeaderPinSelectedPressed,
    tebIEBarMenuNormal, tebIEBarMenuHot, tebIEBarMenuPressed,
    tebNormalGroupBackground,
    tebNormalGroupCollapseNormal, tebNormalGroupCollapseHot,  tebNormalGroupCollapsePressed,
    tebNormalGroupExpandNormal, tebNormalGroupExpandHot,  tebNormalGroupExpandPressed,
    tebNormalGroupHead,
    tebSpecialGroupBackground,
    tebSpecialGroupCollapseSpecial, tebSpecialGroupCollapseHot,  tebSpecialGroupCollapsePressed,
    tebSpecialGroupExpandSpecial, tebSpecialGroupExpandHot,  tebSpecialGroupExpandPressed,
    tebSpecialGroupHead
  );

  // 'Header' theme data
  TThemedHeader = (
    thHeaderDontCare,
    thHeaderRoot,
    thHeaderItemNormal, thHeaderItemHot, thHeaderItemPressed,
    thHeaderItemLeftNormal, thHeaderItemLeftHot, thHeaderItemLeftPressed,
    thHeaderItemRightNormal, thHeaderItemRightHot, thHeaderItemRightPressed,
    thHeaderSortArrowSortedUp, thHeaderSortArrowSortedDown
  );

  // 'ListView' theme data
  TThemedListview = (
    tlListviewDontCare,
    tlListviewRoot,
    tlListItemNormal, tlListItemHot, tlListItemSelected, tlListItemDisabled, tlListItemSelectedNotFocus,
    tlListGroup,
    tlListDetail,
    tlListSortDetail,
    tlEmptyText
  );

  // 'Menu' theme data
  TThemedMenu = (
    tmMenuDontCare,
    tmMenuRoot,
    tmMenuItemNormal, tmMenuItemSelected, tmMenuItemDemoted,
    tmMenuDropDown,
    tmMenuBarItem,
    tmMenuBarDropDown,
    tmChevron,
    tmSeparator
  );

  // 'Page' theme data
  TThemedPage = (
    tpPageDontCare,
    tpPageRoot,
    tpUpNormal, tpUpHot, tpUpPressed, tpUpDisabled,
    tpDownNormal, tpDownHot, tpDownPressed, tpDownDisabled,
    tpUpHorzNormal, tpUpHorzHot, tpUpHorzPressed, tpUpHorzDisabled,
    tpDownHorzNormal, tpDownHorzHot, tpDownHorzPressed, tpDownHorzDisabled
  );

  // 'Progress' theme data
  TThemedProgress = (
    tpProgressDontCare,
    tpProgressRoot,
    tpBar,
    tpBarVert,
    tpChunk,
    tpChunkVert
  );

  // 'Rebar' theme data
  TThemedRebar = (
    trRebarDontCare,
    trRebarRoot,
    trGripper,
    trGripperVert,
    trBandNormal, trBandHot, trBandPressed, trBandDisabled, trBandChecked, trBandHotChecked,
    trChevronNormal, trChevronHot, trChevronPressed, trChevronDisabled,
    trChevronVertNormal, trChevronVertHot, trChevronVertPressed, trChevronVertDisabled
  );

  // 'ScrollBar' theme data
  TThemedScrollBar = (
    tsScrollBarDontCare,
    tsScrollBarRoot,
    tsArrowBtnUpNormal, tsArrowBtnUpHot, tsArrowBtnUpPressed, tsArrowBtnUpDisabled,
    tsArrowBtnDownNormal, tsArrowBtnDownHot, tsArrowBtnDownPressed, tsArrowBtnDownDisabled,
    tsArrowBtnLeftNormal, tsArrowBtnLeftHot, tsArrowBtnLeftPressed, tsArrowBtnLeftDisabled,
    tsArrowBtnRightNormal, tsArrowBtnRightHot, tsArrowBtnRightPressed, tsArrowBtnRightDisabled,
    tsThumbBtnHorzNormal, tsThumbBtnHorzHot, tsThumbBtnHorzPressed, tsThumbBtnHorzDisabled,
    tsThumbBtnVertNormal, tsThumbBtnVertHot, tsThumbBtnVertPressed, tsThumbBtnVertDisabled,
    tsLowerTrackHorzNormal, tsLowerTrackHorzHot, tsLowerTrackHorzPressed, tsLowerTrackHorzDisabled,
    tsUpperTrackHorzNormal, tsUpperTrackHorzHot, tsUpperTrackHorzPressed, tsUpperTrackHorzDisabled,
    tsLowerTrackVertNormal, tsLowerTrackVertHot, tsLowerTrackVertPressed, tsLowerTrackVertDisabled,
    tsUpperTrackVertNormal, tsUpperTrackVertHot, tsUpperTrackVertPressed, tsUpperTrackVertDisabled,
    tsGripperHorzNormal, tsGripperHorzHot, tsGripperHorzPressed, tsGripperHorzDisabled,
    tsGripperVertNormal, tsGripperVertHot, tsGripperVertPressed, tsGripperVertDisabled,
    tsSizeBoxRightAlign, tsSizeBoxLeftAlign
  );

  // 'Spin' theme data
  TThemedSpin = (
    tsSpinDontCare,
    tsSpinRoot,
    tsUpNormal, tsUpHot, tsUpPressed, tsUpDisabled,
    tsDownNormal, tsDownHot, tsDownPressed, tsDownDisabled,
    tsUpHorzNormal, tsUpHorzHot, tsUpHorzPressed, tsUpHorzDisabled,
    tsDownHorzNormal, tsDownHorzHot, tsDownHorzPressed, tsDownHorzDisabled
  );

  // 'StartPanel' theme data
  TThemedStartPanel = (
    tspStartPanelDontCare,
    tspStartPanelRoot,
    tspUserPane,
    tspMorePrograms,
    tspMoreProgramsArrowNormal, tspMoreProgramsArrowHot, tspMoreProgramsArrowPressed,
    tspProgList,
    tspProgListSeparator,
    tspPlacesList,
    tspPlacesListSeparator,
    tspLogOff,
    tspLogOffButtonsNormal, tspLogOffButtonsHot, tspLogOffButtonsPressed,
    tspUserPicture,
    tspPreview
  );

  // 'Status' theme data
  TThemedStatus = (
    tsStatusDontCare,
    tsStatusRoot,
    tsPane,
    tsGripperPane,
    tsGripper
  );

  // 'Tab' theme data
  TThemedTab = (
    ttTabDontCare,
    ttTabRoot,
    ttTabItemNormal, ttTabItemHot, ttTabItemSelected, ttTabItemDisabled, ttTabItemFocused,
    ttTabItemLeftEdgeNormal, ttTabItemLeftEdgeHot, ttTabItemLeftEdgeSelected, ttTabItemLeftEdgeDisabled, ttTabItemLeftEdgeFocused,
    ttTabItemRightEdgeNormal, ttTabItemRightEdgeHot, ttTabItemRightEdgeSelected, ttTabItemRightEdgeDisabled, ttTabItemRightEdgeFocused,
    ttTabItemBothEdgeNormal, ttTabItemBothEdgeHot, ttTabItemBothEdgeSelected, ttTabItemBothEdgeDisabled, ttTabItemBothEdgeFocused,
    ttTopTabItemNormal, ttTopTabItemHot, ttTopTabItemSelected, ttTopTabItemDisabled, ttTopTabItemFocused,
    ttTopTabItemLeftEdgeNormal, ttTopTabItemLeftEdgeHot, ttTopTabItemLeftEdgeSelected, ttTopTabItemLeftEdgeDisabled, ttTopTabItemLeftEdgeFocused,
    ttTopTabItemRightEdgeNormal, ttTopTabItemRightEdgeHot, ttTopTabItemRightEdgeSelected, ttTopTabItemRightEdgeDisabled, ttTopTabItemRightEdgeFocused,
    ttTopTabItemBothEdgeNormal, ttTopTabItemBothEdgeHot, ttTopTabItemBothEdgeSelected, ttTopTabItemBothEdgeDisabled, ttTopTabItemBothEdgeFocused,
    ttPane,
    ttBody
  );

  // 'TaskBand' theme data
  TThemedTaskBand = (
    ttbTaskBandDontCare,
    ttbTaskBandRoot,
    ttbGroupCount,
    ttbFlashButton,
    ttpFlashButtonGroupMenu
  );

  // 'TaskBar' theme data
  TThemedTaskBar = (
    ttTaskBarDontCare,
    ttTaskBarRoot,
    ttbTimeNormal
  );

  // 'ToolBar' theme data
  TThemedToolBar = (
    ttbToolBarDontCare,
    ttbToolBarRoot,
    ttbButtonNormal, ttbButtonHot, ttbButtonPressed, ttbButtonDisabled, ttbButtonChecked, ttbButtonCheckedHot,
    ttbDropDownButtonNormal, ttbDropDownButtonHot, ttbDropDownButtonPressed, ttbDropDownButtonDisabled, ttbDropDownButtonChecked, ttbDropDownButtonCheckedHot,
    ttbSplitButtonNormal, ttbSplitButtonHot, ttbSplitButtonPressed, ttbSplitButtonDisabled, ttbSplitButtonChecked, ttbSplitButtonCheckedHot,
    ttbSplitButtonDropDownNormal, ttbSplitButtonDropDownHot, ttbSplitButtonDropDownPressed, ttbSplitButtonDropDownDisabled, ttbSplitButtonDropDownChecked, ttbSplitButtonDropDownCheckedHot,
    ttbSeparatorNormal, ttbSeparatorHot, ttbSeparatorPressed, ttbSeparatorDisabled, ttbSeparatorChecked, ttbSeparatorCheckedHot,
    ttbSeparatorVertNormal, ttbSeparatorVertHot, ttbSeparatorVertPressed, ttbSeparatorVertDisabled, ttbSeparatorVertChecked, ttbSeparatorVertCheckedHot
  );

  // 'ToolTip' theme data
  TThemedToolTip = (
    tttToolTipDontCare,
    tttToolTipRoot,
    tttStandardNormal, tttStandardLink,
    tttStandardTitleNormal, tttStandardTitleLink,
    tttBaloonNormal, tttBaloonLink,
    tttBaloonTitleNormal, tttBaloonTitleLink,
    tttCloseNormal, tttCloseHot, tttClosePressed
  );

  // 'TrackBar' theme data
  TThemedTrackBar = (
    ttbTrackBarDontCare,
    ttbTrackBarRoot,
    ttbTrack,
    ttbTrackVert,
    ttbThumbNormal, ttbThumbHot, ttbThumbPressed, ttbThumbFocused, ttbThumbDisabled,
    ttbThumbBottomNormal, ttbThumbBottomHot, ttbThumbBottomPressed, ttbThumbBottomFocused, ttbThumbBottomDisabled,
    ttbThumbTopNormal, ttbThumbTopHot, ttbThumbTopPressed, ttbThumbTopFocused, ttbThumbTopDisabled,
    ttbThumbVertNormal, ttbThumbVertHot, ttbThumbVertPressed, ttbThumbVertFocused, ttbThumbVertDisabled,
    ttbThumbLeftNormal, ttbThumbLeftHot, ttbThumbLeftPressed, ttbThumbLeftFocused, ttbThumbLeftDisabled,
    ttbThumbRightNormal, ttbThumbRightHot, ttbThumbRightPressed, ttbThumbRightFocused, ttbThumbRightDisabled,
    ttbThumbTics, 
    ttbThumbTicsVert
  );

  // 'TrayNotify' theme data
  TThemedTrayNotify = (
    ttnTrayNotifyDontCare,
    ttnTrayNotifyRoot,
    ttnBackground,
    ttnAnimBackground
  );

  // 'Treeview' theme data
  TThemedTreeview = (
    ttTreeviewDontCare,
    ttTreeviewRoot,
    ttItemNormal, ttItemHot, ttItemSelected, ttItemDisabled, ttItemSelectedNotFocus,
    ttGlyphClosed, ttGlyphOpened, 
    ttBranch
  );

  // 'Window' theme data
  TThemedWindow = (
    twWindowDontCare,
    twWindowRoot,
    twCaptionActive, twCaptionInactive, twCaptionDisabled,
    twSmallCaptionActive, twSmallCaptionInactive, twSmallCaptionDisabled,
    twMinCaptionActive, twMinCaptionInactive, twMinCaptionDisabled,
    twSmallMinCaptionActive, twSmallMinCaptionInactive, twSmallMinCaptionDisabled,
    twMaxCaptionActive, twMaxCaptionInactive, twMaxCaptionDisabled,
    twSmallMaxCaptionActive, twSmallMaxCaptionInactive, twSmallMaxCaptionDisabled,

    twFrameLeftActive, twFrameLeftInactive,
    twFrameRightActive, twFrameRightInactive,
    twFrameBottomActive, twFrameBottomInactive,
    twSmallFrameLeftActive, twSmallFrameLeftInactive,
    twSmallFrameRightActive, twSmallFrameRightInactive,
    twSmallFrameBottomActive, twSmallFrameBottomInactive,

    twSysButtonNormal, twSysButtonHot, twSysButtonPushed, twSysButtonDisabled,
    twMDISysButtonNormal, twMDISysButtonHot, twMDISysButtonPushed, twMDISysButtonDisabled,
    twMinButtonNormal, twMinButtonHot, twMinButtonPushed, twMinButtonDisabled,
    twMDIMinButtonNormal, twMDIMinButtonHot, twMDIMinButtonPushed, twMDIMinButtonDisabled,
    twMaxButtonNormal, twMaxButtonHot, twMaxButtonPushed, twMaxButtonDisabled,
    twCloseButtonNormal, twCloseButtonHot, twCloseButtonPushed, twCloseButtonDisabled,
    twSmallCloseButtonNormal, twSmallCloseButtonHot, twSmallCloseButtonPushed, twSmallCloseButtonDisabled,
    twMDICloseButtonNormal, twMDICloseButtonHot, twMDICloseButtonPushed, twMDICloseButtonDisabled,
    twRestoreButtonNormal, twRestoreButtonHot, twRestoreButtonPushed, twRestoreButtonDisabled,
    twMDIRestoreButtonNormal, twMDIRestoreButtonHot, twMDIRestoreButtonPushed, twMDIRestoreButtonDisabled,
    twHelpButtonNormal, twHelpButtonHot, twHelpButtonPushed, twHelpButtonDisabled,
    twMDIHelpButtonNormal, twMDIHelpButtonHot, twMDIHelpButtonPushed, twMDIHelpButtonDisabled,

    twHorzScrollNormal, twHorzScrollHot, twHorzScrollPushed, twHorzScrollDisabled,
    twHorzThumbNormal, twHorzThumbHot, twHorzThumbPushed, twHorzThumbDisabled,
    twVertScrollNormal, twVertScrollHot, twVertScrollPushed, twVertScrollDisabled,
    twVertThumbNormal, twVertThumbHot, twVertThumbPushed, twVertThumbDisabled,

    twDialog,
    twCaptionSizingTemplate,
    twSmallCaptionSizingTemplate,
    twFrameLeftSizingTemplate,
    twSmallFrameLeftSizingTemplate,
    twFrameRightSizingTemplate,
    twSmallFrameRightSizingTemplate,
    twFrameBottomSizingTemplate,
    twSmallFrameBottomSizingTemplate
  );

  PThemedElementDetails = ^TThemedElementDetails;
  TThemedElementDetails = record
    Element: TThemedElement;
    Part,
    State: Integer;
  end;

  TImageListItem = class(TObject)
    List: TImageList;
    Name: string;
  end;

  TThemeServices = class(TObject)
  private
    FThemeNames: TStrings;
    FThemeIndex: Integer;
    FThemesDir: string;
    FOnThemeChange: TNotifyEvent;

    FImageLists: TObjectList;
    FEmptyImageList: TImageList;

    function GetThemesAvailable: Boolean;
    function GetThemesEnabled: Boolean;
    function GetThemeCount: Integer;
    function GetThemeNames(Index: Integer): string;
    procedure SetThemeIndex(Value: Integer);
    procedure SetThemesDir(const Value: string);

    procedure DrawEdgedBitmap(DC: QPainterH; const R: TRect;
      const Name: string; Index: Integer); overload;
    procedure DrawEdgedBitmap(DC: QPainterH; const R: TRect; Bmp: TBitmap); overload;
    procedure StretchBitmap(DC: QPainterH; const R: TRect; const Name: string;
      Index: Integer; BmpRect: PRect = nil);
    procedure DrawBitmap(DC: QPainterH; const R: TRect; const Name: string;
      Index: Integer);
    procedure DrawRepeatedBitmap(DC: QPainterH; const R: TRect;
      const Name: string; Index: Integer; const BmpRect: TRect;
      Direction: Integer; Distance: Integer);
    procedure DrawHeader(DC: QPainterH; State: Integer; R: TRect);
    procedure DrawEdgedBitmapTransparent(DC: QPainterH; const R: TRect;
      Bmp: TBitmap; TransparentBody: Boolean); overload;
    procedure DrawEdgedBitmapTransparent(DC: QPainterH; const R: TRect;
      TransparentBody: Boolean; const Name: string; Index: Integer); overload;
    procedure DrawEdgedBitmapBorder(DC: QPainterH; const R: TRect;
      Bmp: TBitmap); overload;
    procedure DrawEdgedBitmapBorder(DC: QPainterH; const R: TRect;
      const Name: string; Index: Integer); overload;
  protected
    procedure LoadThemeNames;
    procedure LoadThemeImages(const Filename: string; Count: Integer);
    procedure DoOnThemeChange; virtual;
  protected
    function GetImages(const Name: string): TImageList;

    procedure DrawPushButton(DC: QPainterH; State: Integer; R: TRect);
    procedure DrawRadioButton(DC: QPainterH; State: Integer; R: TRect);
    procedure DrawCheckBox(DC: QPainterH; State: Integer; R: TRect);
    procedure DrawGroupBox(DC: QPainterH; State: Integer; R: TRect);
    procedure DrawEditText(DC: QPainterH; State: Integer; R: TRect);
    procedure DrawToolBarButton(DC: QPainterH; State: Integer; R: TRect);
    procedure DrawToolBarSeparator(DC: QPainterH; State: Integer;  R: TRect);
    procedure DrawToolBarSeparatorVert(DC: QPainterH; State: Integer; R: TRect);
    procedure DrawScrollBar(DC: QPainterH; State: Integer; R: TRect);
    procedure DrawTab(DC: QPainterH; State: Integer; R: TRect);
    procedure DrawProgressBar(DC: QPainterH; State: Integer; R: TRect);
    procedure DrawComboBox(DC: QPainterH; State: Integer; R: TRect);
    procedure DrawTrackBar(DC: QPainterH; State: Integer; R: TRect);

  protected
    // implemented painting function
    procedure DrawThemeBackground(Element: TThemedElement; DC: QPainterH;
      Part, State: Integer; R: TRect; ClipRect: PRect);
    function IsThemeBackgroundPartiallyTransparent(Element: TThemedElement;
      Part, State: Integer): Boolean;
    procedure DrawThemeParentBackground(Window: QWidgetH; Target: QPainterH;
      ClipRect: PRect);
    procedure DrawThemeText(Element: TThemedElement; DC: QPainterH;
      Part, State: Integer; const S: WideString; Flags, Flags2: Integer;
      const R: TRect);
    procedure GetThemeBackgroundContentRect(Element: TThemedElement; DC: QPainterH;
      Part, State: Integer; const BoundingRect: TRect; RetVal: PRect);
  public
    constructor Create;
    destructor Destroy; override;

    property ThemeIndex: Integer read FThemeIndex write SetThemeIndex;
    property ThemeCount: Integer read GetThemeCount;
    property ThemeNames[Index: Integer]: string read GetThemeNames;
    property ThemesDir: string read FThemesDir write SetThemesDir;


    function GetElementDetails(Detail: TThemedButton): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedClock): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedComboBox): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedEdit): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedExplorerBar): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedHeader): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedListView): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedMenu): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedPage): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedProgress): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedRebar): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedScrollBar): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedSpin): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedStartPanel): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedStatus): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedTab): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedTaskBand): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedTaskBar): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedToolBar): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedToolTip): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedTrackBar): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedTrayNotify): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedTreeview): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedWindow): TThemedElementDetails; overload;

    function ContentRect(DC: QPainterH; Details: TThemedElementDetails; BoundingRect: TRect): TRect; overload;
    function ContentRect(Canvas: TCanvas; Details: TThemedElementDetails; BoundingRect: TRect): TRect; overload;
    procedure DrawEdge(DC: QPainterH; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal;
      ContentRect: PRect = nil); overload;
    procedure DrawEdge(Canvas: TCanvas; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal;
      ContentRect: PRect = nil); overload;
    procedure DrawElement(DC: QPainterH; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect = nil); overload;
    procedure DrawElement(Canvas: TCanvas; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect = nil); overload;
    procedure DrawParentBackground(Window: QWidgetH; Target: QPainterH; Details: PThemedElementDetails; OnlyIfTransparent: Boolean;
      Bounds: PRect = nil); overload;
    procedure DrawParentBackground(Window: QWidgetH; Target: TCanvas; Details: PThemedElementDetails; OnlyIfTransparent: Boolean;
      Bounds: PRect = nil); overload;
    procedure DrawText(DC: QPainterH; Details: TThemedElementDetails; const S: WideString; R: TRect; Flags, Flags2: Cardinal); overload;
    procedure DrawText(Canvas: TCanvas; Details: TThemedElementDetails; const S: WideString; R: TRect; Flags, Flags2: Cardinal); overload;
    function HasTransparentParts(Details: TThemedElementDetails): Boolean;
    procedure PaintBorder(Control: TWinControl; EraseLRCorner: Boolean; Painter: QPainterH = nil);
    procedure UpdateThemes;

    property ThemesAvailable: Boolean read GetThemesAvailable;
    property ThemesEnabled: Boolean read GetThemesEnabled;

    property OnThemeChange: TNotifyEvent read FOnThemeChange write FOnThemeChange;
  end;

  TPainterCanvas = class(TCanvas)
  protected
    procedure BeginPainting; override;
  public
    constructor Create(AHandle: QPainterH);
    destructor Destroy; override;
  end;

function ThemeServices: TThemeServices;

//----------------------------------------------------------------------------------------------------------------------

implementation

const
  File_Button = 'Button.bmp';
  File_CheckBox = 'CheckBox.bmp';
  File_RadioButton = 'RadioButton.bmp';
  File_EditText = 'EditText.bmp';
  File_GroupBox = 'GroupBox.bmp';
  File_ToolButton = 'ToolBarButton.bmp';
  File_ToolButtonSep = 'ToolBarSep.bmp';
  File_ScrollBar = 'ScrollBar.bmp';
  File_Arrows = 'Arrows.bmp';
  File_Tabs = 'Tabs.bmp';
  File_TabBody = 'TabBody.bmp';
  File_ProgressBar = 'ProgressBar.bmp';
  File_ComboBox = 'ComboBox.bmp';
  File_TrackBar = 'TrackBar.bmp';
  File_Header = 'Header.bmp';

  BP_PUSHBUTTON = 1;
  BP_RADIOBUTTON = 2;
  BP_CHECKBOX = 3;
  BP_GROUPBOX = 4;

  EP_EDITTEXT = 1;
  EP_CARET = 2;

  TP_BUTTON = 1;
  TP_DROPDOWNBUTTON = 2;
  TP_SPLITBUTTON = 3;
  TP_SPLITBUTTONDROPDOWN = 4;
  TP_SEPARATOR = 5;
  TP_SEPARATORVERT = 6;

  SBP_ARROWBTN = 1;
  SBP_THUMBBTNHORZ = 2;
  SBP_THUMBBTNVERT = 3;
  SBP_LOWERTRACKHORZ = 4;
  SBP_UPPERTRACKHORZ = 5;
  SBP_LOWERTRACKVERT = 6;
  SBP_UPPERTRACKVERT = 7;
  SBP_GRIPPERHORZ = 8;
  SBP_GRIPPERVERT = 9;
  SBP_SIZEBOX = 10;

  TABP_TABITEM = 1;
  TABP_TABITEMLEFTEDGE = 2;
  TABP_TABITEMRIGHTEDGE = 3;
  TABP_TABITEMBOTHEDGE = 4;
  TABP_TOPTABITEM = 5;
  TABP_TOPTABITEMLEFTEDGE = 6;
  TABP_TOPTABITEMRIGHTEDGE = 7;
  TABP_TOPTABITEMBOTHEDGE = 8;
  TABP_PANE = 9;
  TABP_BODY = 10;

  PP_BAR = 1;
  PP_BARVERT = 2;
  PP_CHUNK = 3;
  PP_CHUNKVERT = 4;

  CP_DROPDOWNBUTTON = 1;

  TKP_TRACK = 1;
  TKP_TRACKVERT = 2;
  TKP_THUMB = 3;
  TKP_THUMBBOTTOM = 4;
  TKP_THUMBTOP = 5;
  TKP_THUMBVERT = 6;
  TKP_THUMBLEFT = 7;
  TKP_THUMBRIGHT = 8;
  TKP_TICS = 9;
  TKP_TICSVERT = 10;

  HP_HEADERITEM = 1;
  HP_HEADERITEMLEFT = 2;
  HP_HEADERITEMRIGHT = 3;
  HP_HEADERSORTARROW = 4;

var
  InternalThemeServices: TThemeServices;

procedure NotImplemented;
begin
  raise Exception.Create('Theming not implemented for this control');
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

procedure SetClipRect(DC: QPainterH; ClipRect: TRect);
begin
  QPainter_save(DC);
  QIntersectClipRect(DC, ClipRect);
end;

procedure ResetClipRect(DC: QPainterH);
begin
  QPainter_restore(DC);
end;

procedure StretchDraw(Canvas: TCanvas; DestR: TRect; Bmp: TBitmap);
begin
  Canvas.StretchDraw(DestR, Bmp);
end;

procedure TThemeServices.DrawEdgedBitmap(DC: QPainterH; const R: TRect; Bmp: TBitmap);
var
  DestR, SrcR: TRect;
  Canvas: TCanvas;
  x2, y2, ix2, iy2: Integer;
  DestBmp: TBitmap;

  procedure DrawPart(vDestR: TRect; const vSourceR: TRect);
  begin
    OffsetRect(vDestR, R.Left, R.Top);
    DestBmp.Width := vSourceR.Right - vSourceR.Left;
    DestBmp.Height := vSourceR.Bottom - vSourceR.Top;
    DestBmp.Canvas.Start;
    try
      bitBlt(DestBmp.Handle, 0, 0, Bmp.Handle, vSourceR.Left, vSourceR.Top,
        DestBmp.Width, DestBmp.Height, RasterOp_CopyROP, True);

      if (vDestR.Right - vDestR.Left = DestBmp.Width) and
         (vDestR.Bottom - vDestR.Top = DestBmp.Height) then
        Canvas.Draw(vDestR.Left, vDestR.Top, DestBmp)
      else
        StretchDraw(Canvas, vDestR, DestBmp);
    finally
      DestBmp.Canvas.Stop;
    end;
  end;

const
  ItemSize = 3;
begin
  x2 := R.Right - R.Left;
  y2 := R.Bottom - R.Top;
  ix2 := Bmp.Width;
  iy2 := Bmp.Height;

  if (ix2 = 0) or (iy2 = 0) or (x2 = 0) or (y2 = 0) or
     (R.Left > R.Right) or (R.Bottom < R.Top) then
    Exit;

  Bmp.Transparent := False;
  Bmp.Canvas.Start;
  DestBmp := TBitmap.Create;
  Canvas := TPainterCanvas.Create(DC);
  try
  // innen
    DestR := Rect(ItemSize, ItemSize, x2 - ItemSize, y2 - ItemSize);
    SrcR := Rect(ItemSize, ItemSize, ix2 - ItemSize, iy2 - ItemSize);
    DrawPart(DestR, SrcR);

   // oben
    DestR := Rect(ItemSize, 0, x2 - ItemSize, ItemSize);
    SrcR := Rect(ItemSize, 0, ix2 - ItemSize, ItemSize);
    DrawPart(DestR, SrcR);

   // unten
    DestR := Rect(ItemSize, y2 - ItemSize, x2 - ItemSize, y2);
    SrcR := Rect(ItemSize, iy2 - ItemSize, ix2 - ItemSize, iy2);
    DrawPart(DestR, SrcR);

   // links
    DestR := Rect(0, ItemSize, ItemSize, y2 - ItemSize);
    SrcR := Rect(0, ItemSize, ItemSize, iy2 - ItemSize);
    DrawPart(DestR, SrcR);

   // rechts
    DestR := Rect(x2 - ItemSize, ItemSize, x2, y2 - ItemSize);
    SrcR := Rect(ix2 - ItemSize, ItemSize, ix2, iy2 - ItemSize);
    DrawPart(DestR, SrcR);

   // Ecke links oben
    SrcR := Rect(0, 0, ItemSize, ItemSize);
    DestR := Rect(0, 0, ItemSize, ItemSize);
    DrawPart(DestR, SrcR);

   // Ecke rechts oben
    DestR := Rect(x2 - ItemSize, 0, x2, ItemSize);
    SrcR := Rect(ix2 - ItemSize, 0, ix2, ItemSize);
    DrawPart(DestR, SrcR);

   // Ecke links unten
    DestR := Rect(0, y2 - ItemSize, ItemSize, y2);
    SrcR := Rect(0, iy2 - ItemSize, ItemSize, iy2);
    DrawPart(DestR, SrcR);

   // Ecke rechts unten
    DestR := Rect(x2 - ItemSize, y2 - ItemSize, x2, y2);
    SrcR := Rect(ix2 - ItemSize, iy2 - ItemSize, ix2, iy2);
    DrawPart(DestR, SrcR);
  finally
    Canvas.Free;
    DestBmp.Free;
    Bmp.Canvas.Stop;
  end;
end;

procedure TThemeServices.DrawEdgedBitmapTransparent(DC: QPainterH; const R: TRect; Bmp: TBitmap;
  TransparentBody: Boolean);
var
  DestR, SrcR: TRect;
  Canvas: TCanvas;
  x2, y2, ix2, iy2: Integer;
  DestBmp: TBitmap;

  procedure DrawPart(const vDestR, vSourceR: TRect);
  begin
    stretchBlt(DestBmp.Handle, @vDestR, Bmp.Handle, @vSourceR,
      RasterOp_CopyROP, True);
  end;

const
  ItemSize = 3;
begin
  x2 := R.Right - R.Left;
  y2 := R.Bottom - R.Top;
  ix2 := Bmp.Width;
  iy2 := Bmp.Height;

  if (ix2 = 0) or (iy2 = 0) or (x2 = 0) or (y2 = 0) or
     (R.Left > R.Right) or (R.Bottom < R.Top) then
    Exit;

  Canvas := TPainterCanvas.Create(DC);
  Bmp.Transparent := False;
  Bmp.Canvas.Start;
  DestBmp := TBitmap.Create;
  try
    DestBmp.Canvas.Brush.Color := clFuchsia;
    DestBmp.Width := x2;
    DestBmp.Height := y2;
    DestBmp.Canvas.Start;

    if not TransparentBody then
    begin
     // innen
      DestR := Rect(ItemSize, ItemSize, x2 - ItemSize, y2 - ItemSize);
      SrcR := Rect(ItemSize, ItemSize, ix2 - ItemSize, iy2 - ItemSize);
      DrawPart(DestR, SrcR);
    end;

   // oben
    DestR := Rect(ItemSize, 0, x2 - ItemSize, ItemSize);
    SrcR := Rect(ItemSize, 0, ix2 - ItemSize, ItemSize);
    DrawPart(DestR, SrcR);

   // unten
    DestR := Rect(ItemSize, y2 - ItemSize, x2 - ItemSize, y2);
    SrcR := Rect(ItemSize, iy2 - ItemSize, ix2 - ItemSize, iy2);
    DrawPart(DestR, SrcR);

   // links
    DestR := Rect(0, ItemSize, ItemSize, y2 - ItemSize);
    SrcR := Rect(0, ItemSize, ItemSize, iy2 - ItemSize);
    DrawPart(DestR, SrcR);

   // rechts
    DestR := Rect(x2 - ItemSize, ItemSize, x2, y2 - ItemSize);
    SrcR := Rect(ix2 - ItemSize, ItemSize, ix2, iy2 - ItemSize);
    DrawPart(DestR, SrcR);

   // Ecke links oben
    SrcR := Rect(0, 0, ItemSize, ItemSize);
    DestR := Rect(0, 0, ItemSize, ItemSize);
    DrawPart(DestR, SrcR);

   // Ecke rechts oben
    DestR := Rect(x2 - ItemSize, 0, x2, ItemSize);
    SrcR := Rect(ix2 - ItemSize, 0, ix2, ItemSize);
    DrawPart(DestR, SrcR);

   // Ecke links unten
    DestR := Rect(0, y2 - ItemSize, ItemSize, y2);
    SrcR := Rect(0, iy2 - ItemSize, ItemSize, iy2);
    DrawPart(DestR, SrcR);

   // Ecke rechts unten
    DestR := Rect(x2 - ItemSize, y2 - ItemSize, x2, y2);
    SrcR := Rect(ix2 - ItemSize, iy2 - ItemSize, ix2, iy2);
    DrawPart(DestR, SrcR);

    DestBmp.TransparentColor := clFuchsia;
    Canvas.Draw(R.Left, R.Top, DestBmp);
  finally
    Canvas.Free;
    DestBmp.Canvas.Stop;
    DestBmp.Free;
    Bmp.Canvas.Stop;
  end;
end;

procedure TThemeServices.DrawEdgedBitmapBorder(DC: QPainterH; const R: TRect; Bmp: TBitmap);
// draws only the border
var
  DestR, SrcR: TRect;
  Matrix: QWMatrixH;
  x2, y2, ix2, iy2: Integer;

  procedure DrawPart(vDestR: TRect; const vSourceR: TRect);
  begin
    if Matrix <> nil then
      QWMatrix_map(Matrix, PRect(@vDesTR), PRect(@vDestR));
    stretchBlt(QPainter_device(DC), @vDestR, Bmp.Handle, @vSourceR,
      RasterOp_CopyROP, False);
  end;

const
  ItemSize = 3;
begin
  x2 := R.Right - R.Left;
  y2 := R.Bottom - R.Top;
  ix2 := Bmp.Width;
  iy2 := Bmp.Height;

  if (ix2 = 0) or (iy2 = 0) or (x2 = 0) or (y2 = 0) or
     (R.Left > R.Right) or (R.Bottom < R.Top) then
    Exit;

  if QPainter_hasWorldXForm(DC) then
    Matrix := QPainter_worldMatrix(DC)
  else
    Matrix := nil;

  Bmp.Canvas.Start;
  try
   // oben
    DestR := Rect(ItemSize, 0, x2 - ItemSize, ItemSize);
    SrcR := Rect(ItemSize, 0, ix2 - ItemSize, ItemSize);
    DrawPart(DestR, SrcR);

   // unten
    DestR := Rect(ItemSize, y2 - ItemSize, x2 - ItemSize, y2);
    SrcR := Rect(ItemSize, iy2 - ItemSize, ix2 - ItemSize, iy2);
    DrawPart(DestR, SrcR);

   // links
    DestR := Rect(0, ItemSize, ItemSize, y2 - ItemSize);
    SrcR := Rect(0, ItemSize, ItemSize, iy2 - ItemSize);
    DrawPart(DestR, SrcR);

   // rechts
    DestR := Rect(x2 - ItemSize, ItemSize, x2, y2 - ItemSize);
    SrcR := Rect(ix2 - ItemSize, ItemSize, ix2, iy2 - ItemSize);
    DrawPart(DestR, SrcR);

   // Ecke links oben
    SrcR := Rect(0, 0, ItemSize, ItemSize);
    DestR := Rect(0, 0, ItemSize, ItemSize);
    DrawPart(DestR, SrcR);

   // Ecke rechts oben
    DestR := Rect(x2 - ItemSize, 0, x2, ItemSize);
    SrcR := Rect(ix2 - ItemSize, 0, ix2, ItemSize);
    DrawPart(DestR, SrcR);

   // Ecke links unten
    DestR := Rect(0, y2 - ItemSize, ItemSize, y2);
    SrcR := Rect(0, iy2 - ItemSize, ItemSize, iy2);
    DrawPart(DestR, SrcR);

   // Ecke rechts unten
    DestR := Rect(x2 - ItemSize, y2 - ItemSize, x2, y2);
    SrcR := Rect(ix2 - ItemSize, iy2 - ItemSize, ix2, iy2);
    DrawPart(DestR, SrcR);
  finally
    Bmp.Canvas.Stop;
  end;
end;

procedure TThemeServices.DrawEdgedBitmap(DC: QPainterH; const R: TRect;
  const Name: string; Index: Integer);
var
  Bmp: TBitmap;
  Img: TImageList;
begin
  Bmp := TBitmap.Create;
  try
    Img := GetImages(Name);
    if (Index >= Img.Count) or (Index < 0) then Index := Img.Count - 1;
    Img.GetBitmap(Index, Bmp);
    DrawEdgedBitmap(DC, R, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TThemeServices.DrawEdgedBitmapTransparent(DC: QPainterH; const R: TRect;
  TransparentBody: Boolean; const Name: string; Index: Integer);
var
  Bmp: TBitmap;
  Img: TImageList;
begin
  Bmp := TBitmap.Create;
  try
    Img := GetImages(Name);
    if (Index >= Img.Count) or (Index < 0) then Index := Img.Count - 1;
    Img.GetBitmap(Index, Bmp);
    DrawEdgedBitmapTransparent(DC, R, Bmp, TransparentBody);
  finally
    Bmp.Free;
  end;
end;

procedure TThemeServices.DrawEdgedBitmapBorder(DC: QPainterH; const R: TRect;
  const Name: string; Index: Integer);
var
  Bmp: TBitmap;
  Img: TImageList;
begin
  Bmp := TBitmap.Create;
  try
    Img := GetImages(Name);
    if (Index >= Img.Count) or (Index < 0) then Index := Img.Count - 1;
    Img.GetBitmap(Index, Bmp);
    DrawEdgedBitmapBorder(DC, R, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TThemeServices.StretchBitmap(DC: QPainterH; const R: TRect;
  const Name: string; Index: Integer; BmpRect: PRect = nil);
var
  Bmp: TBitmap;
  Img: TImageList;
  Canvas: TCanvas;
  Width, Height: Integer;
begin
  Bmp := TBitmap.Create;
  try
    Img := GetImages(Name);
    if (Index >= Img.Count) or (Index < 0) then Index := Img.Count - 1;
    Img.GetBitmap(Index, Bmp);

    if Assigned(BmpRect) then
    begin
      Width := BmpRect.Right - BmpRect.Left;
      Height := BmpRect.Bottom - BmpRect.Top;
      Bmp.Canvas.CopyRect(
        Rect(0, 0, Width, Height),
        Bmp.Canvas,
        Rect(BmpRect.Left, BmpRect.Top, BmpRect.Right, BmpRect.Bottom)
      );
      Bmp.Width := Width;
      Bmp.Height := Height;
    end;

    Bmp.TransparentColor := clFuchsia;
    Canvas := TPainterCanvas.Create(DC);
    try
      StretchDraw(Canvas, R, Bmp);
    finally
      Canvas.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

procedure TThemeServices.DrawBitmap(DC: QPainterH; const R: TRect;
  const Name: string; Index: Integer);
var
  Bmp: TBitmap;
  Img: TImageList;
  Canvas: TCanvas;
begin
  Bmp := TBitmap.Create;
  try
    Img := GetImages(Name);
    if (Index >= Img.Count) or (Index < 0) then Index := Img.Count - 1;
    Img.GetBitmap(Index, Bmp);
    Bmp.TransparentColor := clFuchsia;
    Canvas := TPainterCanvas.Create(DC);
    SetClipRect(DC, R);
    try
      Canvas.Draw(R.Left, R.Top, Bmp);
    finally
      ResetClipRect(DC);
      Canvas.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

procedure TThemeServices.DrawRepeatedBitmap(DC: QPainterH; const R: TRect;
  const Name: string; Index: Integer; const BmpRect: TRect; Direction: Integer;
  Distance: Integer);
  // Direction: 0 = Horz repeat
  //            1 = Vert repeat
var
  Bmp: TBitmap;
  Img: TImageList;
  Canvas: TCanvas;
  x, y, Width, Height: Integer;
begin
  Bmp := TBitmap.Create;
  try
    Img := GetImages(Name);
    if (Index >= Img.Count) or (Index < 0) then Index := Img.Count - 1;
    Img.GetBitmap(Index, Bmp);
    Width := BmpRect.Right - BmpRect.Left;
    Height := BmpRect.Bottom - BmpRect.Top;
    Bmp.Canvas.CopyRect(
      Rect(0, 0, Width, Height),
      Bmp.Canvas,
      Rect(BmpRect.Left, BmpRect.Top, BmpRect.Right, BmpRect.Bottom)
    );
    Bmp.Width := Width;
    Bmp.Height := Height;
    Bmp.TransparentColor := clFuchsia;
    Canvas := TPainterCanvas.Create(DC);
    SetClipRect(Canvas.Handle, R);
    try
      if Direction = 0 then
      begin
        x := R.Left;
        while x <= R.Right do
        begin
          StretchDraw(Canvas, Rect(x, R.Top, x + Width, R.Bottom), Bmp);
          Inc(x, Width + Distance);
        end;
      end
      else
      begin
        y := R.Top;
        while y <= R.Bottom do
        begin
          StretchDraw(Canvas, Rect(R.Left, y, R.Right, y + Height), Bmp);
          Inc(y, Height + Distance);
        end;
      end;
    finally
      ResetClipRect(Canvas.Handle);
      Canvas.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

procedure TThemeServices.DrawPushButton(DC: QPainterH; State: Integer; R: TRect);
begin
  InflateRect(R, -1, -1);
  DrawEdgedBitmapTransparent(DC, R, False, File_Button, State - 1);
end;

procedure TThemeServices.DrawRadioButton(DC: QPainterH; State: Integer; R: TRect);
begin
  Dec(R.Right);
  Dec(R.Bottom);
  StretchBitmap(DC, R, File_RadioButton, State - 1);
end;

procedure TThemeServices.DrawCheckBox(DC: QPainterH; State: Integer; R: TRect);
begin
  StretchBitmap(DC, R, File_CheckBox, State - 1);
end;

procedure TThemeServices.DrawGroupBox(DC: QPainterH; State: Integer; R: TRect);
begin
  DrawEdgedBitmapTransparent(DC, R, True, File_GroupBox, State - 1);
end;

procedure TThemeServices.DrawEditText(DC: QPainterH; State: Integer; R: TRect);
var
  BodyRect: TRect;
begin
  if QPainter_clipRegion(DC) <> nil then
  begin
    BodyRect := R;
    InflateRect(BodyRect, -2, -2);
    if QRegion_contains(QPainter_clipRegion(DC), PRect(@BodyRect)) then
      DrawEdgedBitmap(DC, R, File_EditText, State - 1)
    else
      DrawEdgedBitmapBorder(DC, R, File_EditText, State - 1);
  end
  else
    DrawEdgedBitmap(DC, R, File_EditText, State - 1)
end;

procedure TThemeServices.DrawToolBarButton(DC: QPainterH; State: Integer; R: TRect);
begin
  DrawEdgedBitmapTransparent(DC, R, False, File_ToolButton, State - 1);
end;

procedure TThemeServices.DrawToolBarSeparator(DC: QPainterH; State: Integer; R: TRect);
begin
  Inc(R.Left, (R.Left + R.Right) div 2 - 2);
  DrawBitmap(DC, R, File_ToolButtonSep, 0);
end;

procedure TThemeServices.DrawToolBarSeparatorVert(DC: QPainterH; State: Integer; R: TRect);
begin
  Inc(R.Left, (R.Left + R.Right) div 2 - 2);
  DrawBitmap(DC, R, File_ToolButtonSep, 1);
end;

procedure TThemeServices.DrawScrollBar(DC: QPainterH; State: Integer; R: TRect);
var Index: Integer;
begin
  if State <= 16 then
  begin
    DrawEdgedBitmap(DC, R, File_ScrollBar, (State - 1) mod 8);
    Inc(R.Left, (R.Right - R.Left) div 2 - (12 div 2));
    Inc(R.Top, (R.Bottom - R.Top) div 2 - (12 div 2));
    if (State - 1) mod 4 = 3 then Index := 1 else Index := 0;

    Index := Index + ((State - 1) div 4) * 2;
    case Index of
      0, 1: Inc(R.Left);
      2, 3: Inc(R.Left);
      4, 5: ;
      6, 7: Inc(R.Left);
    end;
    DrawBitmap(DC, R, File_Arrows, Index);
  end
  else
    DrawEdgedBitmap(DC, R, File_ScrollBar, State - 1 - 16);
end;

procedure TThemeServices.DrawTab(DC: QPainterH; State: Integer; R: TRect);
begin
  if State = -1 then
    DrawEdgedBitmap(DC, R, File_TabBody, 0)
  else if State = -2 then
    DrawEdgedBitmap(DC, R, File_TabBody, 1)
  else
    DrawEdgedBitmapTransparent(DC, R, False, File_Tabs, State - 1);
end;

procedure TThemeServices.DrawProgressBar(DC: QPainterH; State: Integer; R: TRect);
begin
  case State of
    1: DrawEdgedBitmapTransparent(DC, R, False, File_ProgressBar, 0);
    2: DrawEdgedBitmapTransparent(DC, R, False, File_ProgressBar, 1);
    3: DrawRepeatedBitmap(DC, R, File_ProgressBar, 2, Rect(0, 0, 8, 21), 0, 2);
    4: DrawRepeatedBitmap(DC, R, File_ProgressBar, 2, Rect(8, 0, 30, 8), 1, 2);
  end;
end;

procedure TThemeServices.DrawComboBox(DC: QPainterH; State: Integer; R: TRect);
begin
  DrawEdgedBitmapTransparent(DC, R, False, File_ComboBox, (State - 1) mod 8);

  Inc(R.Left, (R.Right - R.Left) div 2 - (12 div 2));
  Inc(R.Top, (R.Bottom - R.Top) div 2 - (12 div 2));

  Inc(R.Left);
  Inc(R.Top);
  if State = 4 then
    DrawBitmap(DC, R, File_Arrows, 3)
  else
    DrawBitmap(DC, R, File_Arrows, 2);
end;

procedure TThemeServices.DrawTrackBar(DC: QPainterH; State: Integer; R: TRect);
var
  BmpRect: TRect;
begin
  if State < 0 then
  begin
    if State < -200 then
    begin
      // left/right
      BmpRect := Rect(0, 5, 21, 5 + 11);
      StretchBitmap(DC, R, File_TrackBar, State + 200 - 1, @BmpRect);
    end
    else
    begin
      // bottom/top
      BmpRect := Rect(5, 0, 5 + 11, 21);
      StretchBitmap(DC, R, File_TrackBar, State + 100 - 1, @BmpRect);
    end;
  end
  else
    DrawEdgedBitmapTransparent(DC, R, False, File_TrackBar, State - 1);
end;

procedure TThemeServices.DrawHeader(DC: QPainterH; State: Integer; R: TRect);
begin
  DrawEdgedBitmap(DC, R, File_Header, State - 1);
end;

procedure TThemeServices.DrawThemeBackground(Element: TThemedElement; DC: QPainterH;
  Part, State: Integer; R: TRect; ClipRect: PRect);
begin
  if ClipRect <> nil then
    SetClipRect(DC, ClipRect^);
  try
    case Element of
      teButton:
        begin
          case Part of
            BP_PUSHBUTTON:
              DrawPushButton(DC, State, R);
            BP_RADIOBUTTON:
              DrawRadioButton(DC, State, R);
            BP_CHECKBOX:
              DrawCheckBox(DC, State, R);
            BP_GROUPBOX:
              DrawGroupBox(DC, State, R);
          end;
        end;

      teEdit:
        begin
          case Part of
            EP_EDITTEXT:
              DrawEditText(DC, State, R);
            EP_CARET:
              DrawEditText(DC, State, R);
          end;
        end;

      teToolBar:
        begin
          if Part in [TP_BUTTON, TP_DROPDOWNBUTTON, TP_SPLITBUTTON] then
            if State = 1 then Exit;
          case Part of
            TP_BUTTON:
              DrawToolBarButton(DC, State, R);
            TP_DROPDOWNBUTTON:
              DrawToolBarButton(DC, State + Ord(ttbDropDownButtonNormal) - Ord(ttbButtonNormal), R);
            TP_SPLITBUTTON:
              DrawToolBarButton(DC, State + Ord(ttbSplitButtonNormal) - Ord(ttbButtonNormal), R);
            TP_SEPARATOR:
              DrawToolBarSeparator(DC, State, R);
            TP_SEPARATORVERT:
              DrawToolBarSeparatorVert(DC, State, R);
          end;
        end;

      teScrollBar:
        begin
          DrawScrollBar(DC, State, R);
        end;

      teTab:
        begin
          case Part of
            TABP_PANE:
              DrawTab(DC, -1, R);
            TABP_BODY:
              DrawTab(DC, -2, R);
          else
            DrawTab(DC, State, R);
          end;
        end;


      teProgress:
        begin
          case Part of
            PP_BAR:
              DrawProgressBar(DC, 1, R);
            PP_BARVERT:
              DrawProgressBar(DC, 2, R);
            PP_CHUNK:
              DrawProgressBar(DC, 3, R);
            PP_CHUNKVERT:
              DrawProgressBar(DC, 4, R);
          end;
        end;

      teComboBox:
        begin
          case Part of
            CP_DROPDOWNBUTTON:
              DrawComboBox(DC, State, R);
          end;
        end;

      teTrackBar:
        begin
          case Part of
            TKP_THUMBBOTTOM, TKP_THUMBTOP:
              DrawTrackBar(DC, -100 + State, R);
            TKP_THUMBLEFT, TKP_THUMBRIGHT:
              DrawTrackBar(DC, -200 + State, R);
          else
            DrawTrackBar(DC, State, R);
          end;
        end;

      teHeader:
        begin
          DrawHeader(DC, State, R);
        end;

    end;
  finally
    if ClipRect <> nil then
      ResetClipRect(DC);
  end;
end;

function TThemeServices.IsThemeBackgroundPartiallyTransparent(Element: TThemedElement;
  Part, State: Integer): Boolean;
begin
  Result := False;
end;

type
  TOpenWidgetControl = class(TWidgetControl);

procedure TThemeServices.DrawThemeParentBackground(Window: QWidgetH; Target: QPainterH;
  ClipRect: PRect);
var
  Canvas: TCanvas;
  R, CR: TRect;
  Pixmap: QPixmapH;
begin
  QWidget_geometry(Window, @R);
  Window := QWidget_parentWidget(Window);
  if Window = nil then Exit;
  Canvas := TPainterCanvas.Create(Target);
  if ClipRect <> nil then
    SetClipRect(Canvas.Handle, ClipRect^);
  try
    CR := R;
    OffsetRect(CR, -CR.Left, -CR.Top);
    Canvas.Brush.Color := QColorColor(QWidget_backgroundColor(Window));
    Pixmap := QWidget_backgroundPixmap(Window);
    if Pixmap <> nil then
      QPainter_drawPixmap(Target, 0, 0, Pixmap, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top)
    else
      Canvas.FillRect(CR);
  finally
    if ClipRect <> nil then
      ResetClipRect(Canvas.Handle);
    Canvas.Free;
  end;
end;

procedure TThemeServices.DrawThemeText(Element: TThemedElement; DC: QPainterH;
  Part, State: Integer; const S: WideString; Flags, Flags2: Integer;
  const R: TRect);
var
  Canvas: TCanvas;
begin
  QPainter_save(DC);
  Canvas := TPainterCanvas.Create(DC);
  try
    case Element of
      teButton:
        begin
          case Part of
            BP_GROUPBOX:
              Canvas.Font.Color := clBlue;
          end;
        end;
    end;
    Canvas.TextRect(R, R.Left, R.Top, S, Flags);
  finally
    Canvas.Free;
    QPainter_restore(DC);
  end;
end;

procedure TThemeServices.GetThemeBackgroundContentRect(Element: TThemedElement; DC: QPainterH;
  Part, State: Integer; const BoundingRect: TRect; RetVal: PRect);
begin
  if RetVal = nil then
    Exit;

  case Element of
    teProgress:
      begin
        RetVal^ := BoundingRect;
        InflateRect(RetVal^, -2, -2);
      end;
  else
    NotImplemented;
  end;
end;

function ThemeServices: TThemeServices;
begin
  if InternalThemeServices = nil then
    InternalThemeServices := TThemeServices.Create;
  Result := InternalThemeServices;
end;

constructor TThemeServices.Create;
begin
  inherited Create;
  FThemesDir := ExtractFilePath(ParamStr(0));
  FImageLists := TObjectList.Create;
  FEmptyImageList := TImageList.Create(nil);
  FThemeIndex := -1;
  FThemeNames := TStringList.Create;
  LoadThemeNames;
  UpdateThemes;
end;

destructor TThemeServices.Destroy;
begin
  FThemeNames.Clear;
  FThemeIndex := -1;
  UpdateThemes; // destroy graphic objects
  FThemeNames.Free;
  FEmptyImageList.Free;
  FImageLists.Free;
  inherited Destroy;
end;

function TThemeServices.GetThemesAvailable: Boolean;
begin
  Result := FThemeNames.Count > 0;
end;

function TThemeServices.GetThemesEnabled: Boolean;
begin
  Result := ThemesAvailable and (ThemeIndex <> -1);
end;

procedure TThemeServices.DoOnThemeChange;
begin
  if Assigned(FOnThemeChange) then
    FOnThemeChange(Self);
end;

procedure TThemeServices.LoadThemeNames;
var
  sr: TSearchRec;
begin
  FThemeIndex := -1;
  FThemeNames.Clear;
  if FindFirst(ThemesDir + 'Themes' + PathDelim + '*', faAnyFile, sr) = 0 then
  try
    repeat
      if (sr.Attr and faDirectory <> 0) and (sr.Name <> '.') and (sr.Name <> '..') then
        FThemeNames.Add(sr.Name);
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
  if FThemeNames.Count > 0 then
    FThemeIndex := 0;
end;

function TThemeServices.GetThemeCount: Integer;
begin
  Result := FThemeNames.Count;
end;

function TThemeServices.GetThemeNames(Index: Integer): string;
begin
  Result := FThemeNames[Index];
end;

procedure TThemeServices.SetThemeIndex(Value: Integer);
begin
  if Value <> FThemeIndex then
  begin
    FThemeIndex := Value;
    UpdateThemes;
  end;
end;

procedure TThemeServices.SetThemesDir(const Value: string);
begin
  if Value <> FThemesDir then
  begin
    FThemesDir := IncludeTrailingPathDelimiter(Value);
    LoadThemeNames;
    UpdateThemes;
  end;
end;

procedure TThemeServices.LoadThemeImages(const Filename: string; Count: Integer);
var
  Item: TImageListItem;
  Bmp: TBitmap;
begin
  if FileExists(ThemesDir + 'Themes' + PathDelim + ThemeNames[FThemeIndex] + PathDelim + Filename) then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.LoadFromFile(ThemesDir + 'Themes' + PathDelim + ThemeNames[FThemeIndex] + PathDelim + Filename);
      Item := TImageListItem.Create;
      FImageLists.Add(Item);
      Item.Name := Filename;
      Item.List := TImageList.Create(nil);
      Item.List.Masked := False;
      Item.List.Width := Bmp.Width div Count;
      Item.List.Height := Bmp.Height;
      Item.List.Add(Bmp, nil);
    finally
      Bmp.Free;
    end;
  end;
end;

procedure TThemeServices.UpdateThemes;
var
  i: Integer;
begin
  FImageLists.Clear;
  if not ThemesEnabled then
    Exit;

  LoadThemeImages(File_Button, 5);
  LoadThemeImages(File_CheckBox, 12);
  LoadThemeImages(File_RadioButton, 8);
  LoadThemeImages(File_GroupBox, 3);
  LoadThemeImages(File_EditText, 1);
  LoadThemeImages(File_ToolButton, 6 * 3);
  LoadThemeImages(File_ToolButtonSep, 2);
  LoadThemeImages(File_ScrollBar, 34);
  LoadThemeImages(File_Arrows, 4 * 2);
  LoadThemeImages(File_Tabs, 8 * 5);
  LoadThemeImages(File_TabBody, 2);
  LoadThemeImages(File_ProgressBar, 3);
  LoadThemeImages(File_ComboBox, 4);
  LoadThemeImages(File_TrackBar, 34);
  LoadThemeImages(File_Header, 11);

  for i := 0 to Screen.CustomFormCount - 1 do
  begin
    if TForm(Screen.CustomForms[i]).Visible then
      TForm(Screen.CustomForms[i]).Invalidate;
  end;

  DoOnThemeChange;
end;

function TThemeServices.GetImages(const Name: string): TImageList;
var
  i: Integer;
  Item: TImageListItem;
begin
  Result := FEmptyImageList;
  for i := 0 to FImageLists.Count - 1 do
  begin
    Item := TImageListItem(FImageLists[i]);
    if Item.Name = Name then
    begin
      Result := Item.List;
      Break;
    end;
  end;
end;

function TThemeServices.GetElementDetails(Detail: TThemedButton): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teButton;
  with Result do
  begin
    case Detail of
      tbPushButtonNormal..tbPushButtonDefaulted:
        begin
          Part := BP_PUSHBUTTON;
          Base := Ord(tbPushButtonNormal);
        end;
      tbRadioButtonUncheckedNormal..tbRadioButtonCheckedDisabled:
        begin
          Part := BP_RADIOBUTTON;
          Base := Ord(tbRadioButtonUncheckedNormal);
        end;
      tbCheckBoxUncheckedNormal..tbCheckBoxMixedDisabled:
        begin
          Part := BP_CHECKBOX;
          Base := Ord(tbCheckBoxUncheckedNormal);
        end;
      tbGroupBoxNormal..tbGroupBoxDisabled:
        begin
          Part := BP_GROUPBOX;
          Base := Ord(tbGroupBoxNormal);
        end;
      tbUserButton:
        begin
          Part := BP_GROUPBOX;
          Base := Ord(tbUserButton);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

function TThemeServices.GetElementDetails(Detail: TThemedClock): TThemedElementDetails;
begin
  NotImplemented;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedComboBox): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teComboBox;
  with Result do
  begin
    case Detail of
      tcDropDownButtonNormal..tcDropDownButtonDisabled:
        begin
          Part := CP_DROPDOWNBUTTON;
          Base := Ord(tcDropDownButtonNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

function TThemeServices.GetElementDetails(Detail: TThemedEdit): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teEdit;
  with Result do
  begin
    case Detail of
      teEditTextNormal..teEditTextAssist:
        begin
          Part := EP_EDITTEXT;
          Base := Ord(teEditTextNormal);
        end;
      teEditCaret:
        begin
          Part := EP_CARET;
          Base := Ord(teEditCaret);
          NotImplemented;
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

function TThemeServices.GetElementDetails(Detail: TThemedExplorerBar): TThemedElementDetails;
{var
  Base: Integer;}
begin
{  Result.Element := teExplorerBar;
  with Result do
  begin
    case Detail of
      tebHeaderBackgroundNormal..tebHeaderBackgroundPressed:
        begin
          Part := EBP_HEADERBACKGROUND;
          Base := Ord(tebHeaderBackgroundNormal);
        end;
      tebHeaderCloseNormal..tebHeaderClosePressed:
        begin
          Part := EBP_HEADERCLOSE;
          Base := Ord(tebHeaderCloseNormal);
        end;
      tebHeaderPinNormal..tebHeaderPinSelectedPressed:
        begin
          Part := EBP_HEADERPIN;
          Base := Ord(tebHeaderPinSelectedNormal);
        end;
      tebIEBarMenuNormal..tebIEBarMenuPressed:
        begin
          Part := EBP_IEBARMENU;
          Base := Ord(tebIEBarMenuNormal);
        end;
      tebNormalGroupBackground:
        begin
          Part := EBP_NORMALGROUPBACKGROUND;
          Base := Ord(tebNormalGroupBackground);
        end;
      tebNormalGroupCollapseNormal..tebNormalGroupCollapsePressed:
        begin
          Part := EBP_NORMALGROUPCOLLAPSE;
          Base := Ord(tebNormalGroupCollapseNormal);
        end;
      tebNormalGroupExpandNormal..tebNormalGroupExpandPressed:
        begin
          Part := EBP_NORMALGROUPEXPAND;
          Base := Ord(tebNormalGroupExpandNormal);
        end;
      tebNormalGroupHead:
        begin
          Part := EBP_NORMALGROUPHEAD;
          Base := Ord(tebNormalGroupHead);
        end;
      tebSpecialGroupBackground:
        begin
          Part := EBP_SPECIALGROUPBACKGROUND;
          Base := Ord(tebSpecialGroupBackground);
        end;
      tebSpecialGroupCollapseSpecial..tebSpecialGroupCollapsePressed:
        begin
          Part := EBP_SPECIALGROUPCOLLAPSE;
          Base := Ord(tebSpecialGroupCollapseSpecial);
        end;
      tebSpecialGroupExpandSpecial..tebSpecialGroupExpandPressed:
        begin
          Part := EBP_SPECIALGROUPEXPAND;
          Base := Ord(tebSpecialGroupExpandSpecial);
        end;
      tebSpecialGroupHead:
        begin
          Part := EBP_SPECIALGROUPHEAD;
          Base := Ord(tebSpecialGroupHead);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;}
  NotImplemented;
end;

function TThemeServices.GetElementDetails(Detail: TThemedHeader): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teHeader;
  with Result do
  begin
    Base := Ord(thHeaderItemNormal);
    case Detail of
      thHeaderItemNormal..thHeaderItemPressed:
        begin
          Part := HP_HEADERITEM;
          //Base := Ord(thHeaderItemNormal);
        end;
      thHeaderItemLeftNormal..thHeaderItemLeftPressed:
        begin
          Part := HP_HEADERITEMLEFT;
          //Base := Ord(thHeaderItemLeftNormal);
        end;
      thHeaderItemRightNormal..thHeaderItemRightPressed:
        begin
          Part := HP_HEADERITEMRIGHT;
          //Base := Ord(thHeaderItemRightNormal);
        end;
      thHeaderSortArrowSortedUp..thHeaderSortArrowSortedDown:
        begin
          Part := HP_HEADERSORTARROW;
          //Base := Ord(thHeaderSortArrowSortedUp);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

function TThemeServices.GetElementDetails(Detail: TThemedListview): TThemedElementDetails;
{var
  Base: Integer;}
begin
{  Result.Element := teListView;
  with Result do
  begin
    case Detail of
      tlListItemNormal..tlListItemSelectedNotFocus:
        begin
          Part := LVP_LISTITEM;
          Base := Ord(tlListItemNormal);
        end;
      tlListGroup:
        begin
          Part := LVP_LISTGROUP;
          Base := Ord(tlListGroup);
        end;
      tlListDetail:
        begin
          Part := LVP_LISTDETAIL;
          Base := Ord(tlListDetail);
        end;
      tlListSortDetail:
        begin
          Part := LVP_LISTSORTEDDETAIL;
          Base := Ord(tlListSortDetail);
        end;
      tlEmptyText:
        begin
          Part := LVP_EMPTYTEXT;
          Base := Ord(tlEmptyText);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;}
  NotImplemented;
end;

function TThemeServices.GetElementDetails(Detail: TThemedMenu): TThemedElementDetails;
{var
  Base: Integer;}
begin
{  Result.Element := teMenu;
  with Result do
  begin
    case Detail of
      tmMenuItemNormal..tmMenuItemDemoted:
        begin
          Part := MP_MENUITEM;
          Base := Ord(tmMenuItemNormal);
        end;
      tmMenuDropDown:
        begin
          Part := MP_MENUDROPDOWN;
          Base := Ord(tmMenuDropDown);
        end;
      tmMenuBarItem:
        begin
          Part := MP_MENUBARITEM;
          Base := Ord(tmMenuBarItem);
        end;
      tmMenuBarDropDown:
        begin
          Part := MP_MENUBARDROPDOWN;
          Base := Ord(tmMenuBarDropDown);
        end;
      tmChevron:
        begin
          Part := MP_CHEVRON;
          Base := Ord(tmChevron);
        end;
      tmSeparator:
        begin
          Part := MP_SEPARATOR;
          Base := Ord(tmSeparator);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;}
  NotImplemented;
end;

function TThemeServices.GetElementDetails(Detail: TThemedPage): TThemedElementDetails;
{var
  Base: Integer;}
begin
{  Result.Element := tePage;
  with Result do
  begin
    case Detail of
      tpUpNormal..tpUpDisabled:
        begin
          Part := PGRP_UP;
          Base := Ord(tpUpNormal);
        end;
      tpDownNormal..tpDownDisabled:
        begin
          Part := PGRP_DOWN;
          Base := Ord(tpDownNormal);
        end;
      tpUpHorzNormal..tpUpHorzDisabled:
        begin
          Part := PGRP_UPHORZ;
          Base := Ord(tpUpHorzNormal);
        end;
      tpDownHorzNormal..tpDownHorzDisabled:
        begin
          Part := PGRP_DOWNHORZ;
          Base := Ord(tpDownHorzNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;}
  NotImplemented;
end;

function TThemeServices.GetElementDetails(Detail: TThemedProgress): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teProgress;
  with Result do
  begin
    case Detail of
      tpBar:
        begin
          Part := PP_BAR;
          Base := Ord(tpBar);
        end;
      tpBarVert:
        begin
          Part := PP_BARVERT;
          Base := Ord(tpBarVert);
        end;
      tpChunk:
        begin
          Part := PP_CHUNK;
          Base := Ord(tpChunk);
        end;
      tpChunkVert:
        begin
          Part := PP_CHUNKVERT;
          Base := Ord(tpChunkVert);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

function TThemeServices.GetElementDetails(Detail: TThemedRebar): TThemedElementDetails;
{var
  Base: Integer;}
begin
{  Result.Element := teRebar;
  with Result do
  begin
    case Detail of
      trGripper:
        begin
          Part := RP_GRIPPER;
          Base := Ord(trGripper);
        end;
      trGripperVert:
        begin
          Part := RP_GRIPPERVERT;
          Base := Ord(trGripperVert);
        end;
      trBandNormal..trBandHotChecked:
        begin
          Part := RP_BAND;
          Base := Ord(trBandNormal);
        end;
      trChevronNormal..trChevronDisabled:
        begin
          Part := RP_CHEVRON;
          Base := Ord(trChevronNormal);
        end;
      trChevronVertNormal..trChevronVertDisabled:
        begin
          Part := RP_CHEVRONVERT;
          Base := Ord(trChevronVertNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;}
  NotImplemented;
end;

function TThemeServices.GetElementDetails(Detail: TThemedScrollBar): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teScrollBar;
  with Result do
  begin
    case Detail of
      tsArrowBtnUpNormal..tsArrowBtnRightDisabled:
        begin
          Part := SBP_ARROWBTN;
          //Base := Ord(tsArrowBtnUpNormal);
        end;
      tsThumbBtnHorzNormal..tsThumbBtnHorzDisabled:
        begin
          Part := SBP_THUMBBTNHORZ;
          //Base := Ord(tsThumbBtnHorzNormal);
        end;
      tsThumbBtnVertNormal..tsThumbBtnVertDisabled:
        begin
          Part := SBP_THUMBBTNVERT;
          //Base := Ord(tsThumbBtnVertNormal);
        end;
      tsLowerTrackHorzNormal..tsLowerTrackHorzDisabled:
        begin
          Part := SBP_LOWERTRACKHORZ;
          //Base := Ord(tsLowerTrackHorzNormal);
        end;
      tsUpperTrackHorzNormal..tsUpperTrackHorzDisabled:
        begin
          Part := SBP_UPPERTRACKHORZ;
          //Base := Ord(tsUpperTrackHorzNormal);
        end;
      tsLowerTrackVertNormal..tsLowerTrackVertDisabled:
        begin
          Part := SBP_LOWERTRACKVERT;
          //Base := Ord(tsLowerTrackVertNormal);
        end;
      tsUpperTrackVertNormal..tsUpperTrackVertDisabled:
        begin
          Part := SBP_UPPERTRACKVERT;
          //Base := Ord(tsUpperTrackVertNormal);
        end;
      tsGripperHorzNormal..tsGripperHorzDisabled:
        begin
          Part := SBP_GRIPPERHORZ;
          //Base := Ord(tsGripperHorzNormal);
        end;
      tsGripperVertNormal..tsGripperVertDisabled:
        begin
          Part := SBP_GRIPPERVERT;
          //Base := Ord(tsGripperVertNormal);
        end;
      tsSizeBoxRightAlign..tsSizeBoxLeftAlign:
        begin
          Part := SBP_SIZEBOX;
          //Base := Ord(tsSizeBoxRightAlign);
        end;
    else
      Part := 0;
      //Base := 0;
    end;
    Base := Ord(tsArrowBtnUpNormal);
    State := Ord(Detail) - Base + 1;
  end;
end;

function TThemeServices.GetElementDetails(Detail: TThemedSpin): TThemedElementDetails;
{var
  Base: Integer;}
begin
{  Result.Element := teSpin;
  with Result do
  begin
    case Detail of
      tsUpNormal..tsUpDisabled:
        begin
          Part := SPNP_UP;
          Base := Ord(tsUpNormal);
        end;
      tsDownNormal..tsDownDisabled:
        begin
          Part := SPNP_DOWN;
          Base := Ord(tsDownNormal);
        end;
      tsUpHorzNormal..tsUpHorzDisabled:
        begin
          Part := SPNP_UPHORZ;
          Base := Ord(tsUpHorzNormal);
        end;
      tsDownHorzNormal..tsDownHorzDisabled:
        begin
          Part := SPNP_DOWNHORZ;
          Base := Ord(tsDownHorzNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;}
  NotImplemented;
end;

function TThemeServices.GetElementDetails(Detail: TThemedStartPanel): TThemedElementDetails;
{var
  Base: Integer;}
begin
{  Result.Element := teStartPanel;
  with Result do
  begin
    case Detail of
      tspUserPane:
        begin
          Part := SPP_USERPANE;
          Base := Ord(tspUserPane);
        end;
      tspMorePrograms:
        begin
          Part := SPP_MOREPROGRAMS;
          Base := Ord(tspMorePrograms);
        end;
      tspMoreProgramsArrowNormal..tspMoreProgramsArrowPressed:
        begin
          Part := SPP_MOREPROGRAMSARROW;
          Base := Ord(tspMoreProgramsArrowNormal);
        end;
      tspProgList:
        begin
          Part := SPP_PROGLIST;
          Base := Ord(tspProgList);
        end;
      tspProgListSeparator:
        begin
          Part := SPP_PROGLISTSEPARATOR;
          Base := Ord(tspProgListSeparator);
        end;
      tspPlacesList:
        begin
          Part := SPP_PLACESLIST;
          Base := Ord(tspPlacesList);
        end;
      tspPlacesListSeparator:
        begin
          Part := SPP_PLACESLISTSEPARATOR;
          Base := Ord(tspPlacesListSeparator);
        end;
      tspLogOff:
        begin
          Part := SPP_LOGOFF;
          Base := Ord(tspLogOff);
        end;
      tspLogOffButtonsNormal..tspLogOffButtonsPressed:
        begin
          Part := SPP_LOGOFFBUTTONS;
          Base := Ord(tspLogOffButtonsNormal);
        end;
      tspUserPicture:
        begin
          Part := SPP_USERPICTURE;
          Base := Ord(tspUserPicture);
        end;
      tspPreview:
        begin
          Part := SPP_PREVIEW;
          Base := Ord(tspPreview);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;}
  NotImplemented;
end;

function TThemeServices.GetElementDetails(Detail: TThemedStatus): TThemedElementDetails;
{var
  Base: Integer;}
begin
{  Result.Element := teStatus;
  with Result do
  begin
    case Detail of
      tsPane:
        begin
          Part := SP_PANE;
          Base := Ord(tsPane);
        end;
      tsGripperPane:
        begin
          Part := SP_GRIPPERPANE;
          Base := Ord(tsGripperPane);
        end;
      tsGripper:
        begin
          Part := SP_GRIPPER;
          Base := Ord(tsGripper);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;}
  NotImplemented;
end;

function TThemeServices.GetElementDetails(Detail: TThemedTab): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teTab;
  with Result do
  begin
    Base := Ord(ttTabItemNormal);
    case Detail of
      ttTabItemNormal..ttTabItemFocused:
        begin
          Part := TABP_TABITEM;
          //Base := Ord(ttTabItemNormal);
        end;
      ttTabItemLeftEdgeNormal..ttTabItemLeftEdgeFocused:
        begin
          Part := TABP_TABITEMLEFTEDGE;
          //Base := Ord(ttTabItemLeftEdgeNormal);
        end;
      ttTabItemRightEdgeNormal..ttTabItemRightEdgeFocused:
        begin
          Part := TABP_TABITEMRIGHTEDGE;
          //Base := Ord(ttTabItemRightEdgeNormal);
        end;
      ttTabItemBothEdgeNormal..ttTabItemBothEdgeFocused:
        begin
          Part := TABP_TABITEMBOTHEDGE;
          //Base := Ord(ttTabItemBothEdgeNormal);
        end;
      ttTopTabItemNormal..ttTopTabItemFocused:
        begin
          Part := TABP_TOPTABITEM;
          //Base := Ord(ttTopTabItemNormal);
        end;
      ttTopTabItemLeftEdgeNormal..ttTopTabItemLeftEdgeFocused:
        begin
          Part := TABP_TOPTABITEMLEFTEDGE;
          //Base := Ord(ttTopTabItemLeftEdgeNormal);
        end;
      ttTopTabItemRightEdgeNormal..ttTopTabItemRightEdgeFocused:
        begin
          Part := TABP_TOPTABITEMRIGHTEDGE;
          //Base := Ord(ttTopTabItemRightEdgeNormal);
        end;
      ttTopTabItemBothEdgeNormal..ttTopTabItemBothEdgeFocused:
        begin
          Part := TABP_TOPTABITEMBOTHEDGE;
          //Base := Ord(ttTopTabItemBothEdgeNormal);
        end;
      ttPane:
        begin
          Part := TABP_PANE;
          Base := Ord(ttPane);
        end;
      ttBody:
        begin
          Part := TABP_BODY;
          Base := Ord(ttBody);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedTaskBand): TThemedElementDetails;
{var
  Base: Integer;}
begin
{  Result.Element := teTaskBand;
  with Result do
  begin
    case Detail of
      ttbGroupCount:
        begin
          Part := TDP_GROUPCOUNT;
          Base := Ord(ttbGroupCount);
        end;
      ttbFlashButton:
        begin
          Part := TDP_FLASHBUTTON;
          Base := Ord(ttbFlashButton);
        end;
      ttpFlashButtonGroupMenu:
        begin
          Part := TDP_FLASHBUTTONGROUPMENU;
          Base := Ord(ttpFlashButtonGroupMenu);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;}
  NotImplemented;
end;

function TThemeServices.GetElementDetails(Detail: TThemedTaskBar): TThemedElementDetails;
{var
  Base: Integer;}
begin
{  Result.Element := teTaskBar;
  with Result do
  begin
    case Detail of
      ttbTimeNormal:
        begin
          Part := CLP_TIME;
          Base := Ord(ttbTimeNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;}
  NotImplemented;
end;

function TThemeServices.GetElementDetails(Detail: TThemedToolBar): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teToolBar;
  with Result do
  begin
    case Detail of
      ttbButtonNormal..ttbButtonCheckedHot:
        begin
          Part := TP_BUTTON;
          Base := Ord(ttbButtonNormal);
        end;
      ttbDropDownButtonNormal..ttbDropDownButtonCheckedHot:
        begin
          Part := TP_DROPDOWNBUTTON;
          Base := Ord(ttbDropDownButtonNormal);
        end;
      ttbSplitButtonNormal..ttbSplitButtonCheckedHot:
        begin
          Part := TP_SPLITBUTTON;
          Base := Ord(ttbSplitButtonNormal);
        end;
      ttbSplitButtonDropDownNormal..ttbSplitButtonDropDownCheckedHot:
        begin
          Part := TP_SPLITBUTTONDROPDOWN;
          Base := Ord(ttbSplitButtonDropDownNormal);
        end;
      ttbSeparatorNormal..ttbSeparatorCheckedHot:
        begin
          Part := TP_SEPARATOR;
          Base := Ord(ttbSeparatorNormal);
        end;
      ttbSeparatorVertNormal..ttbSeparatorVertCheckedHot:
        begin
          Part := TP_SEPARATORVERT;
          Base := Ord(ttbSeparatorVertNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

function TThemeServices.GetElementDetails(Detail: TThemedToolTip): TThemedElementDetails;
{var
  Base: Integer;}
begin
{  Result.Element := teToolTip;
  with Result do
  begin
    case Detail of
      tttStandardNormal..tttStandardLink:
        begin
          Part := TTP_STANDARD;
          Base := Ord(tttStandardNormal);
        end;
      tttStandardTitleNormal..tttStandardTitleLink:
        begin
          Part := TTP_STANDARDTITLE;
          Base := Ord(tttStandardTitleNormal);
        end;
      tttBaloonNormal..tttBaloonLink:
        begin
          Part := TTP_BALLOON;
          Base := Ord(tttBaloonNormal);
        end;
      tttBaloonTitleNormal..tttBaloonTitleLink:
        begin
          Part := TTP_BALLOONTITLE;
          Base := Ord(tttBaloonTitleNormal);
        end;
      tttCloseNormal..tttClosePressed:
        begin
          Part := TTP_CLOSE;
          Base := Ord(tttCloseNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;}
  NotImplemented;
end;

function TThemeServices.GetElementDetails(Detail: TThemedTrackBar): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teTrackBar;
  with Result do
  begin
    Base := Ord(ttbTrack);
    case Detail of
      ttbTrack:
        begin
          Part := TKP_TRACK;
          //Base := Ord(ttbTrack);
        end;
      ttbTrackVert:
        begin
          Part := TKP_TRACKVERT;
          //Base := Ord(ttbTrackVert);
        end;
      ttbThumbNormal..ttbThumbDisabled:
        begin
          Part := TKP_THUMB;
          //Base := Ord(ttbThumbNormal);
        end;
      ttbThumbBottomNormal..ttbThumbBottomDisabled:
        begin
          Part := TKP_THUMBBOTTOM;
          //Base := Ord(ttbThumbBottomNormal);
        end;
      ttbThumbTopNormal..ttbThumbTopDisabled:
        begin
          Part := TKP_THUMBTOP;
          //Base := Ord(ttbThumbTopNormal);
        end;
      ttbThumbVertNormal..ttbThumbVertDisabled:
        begin
          Part := TKP_THUMBVERT;
          //Base := Ord(ttbThumbVertNormal);
        end;
      ttbThumbLeftNormal..ttbThumbLeftDisabled:
        begin
          Part := TKP_THUMBLEFT;
          //Base := Ord(ttbThumbLeftNormal);
        end;
      ttbThumbRightNormal..ttbThumbRightDisabled:
        begin
          Part := TKP_THUMBRIGHT;
          //Base := Ord(ttbThumbRightNormal);
        end;
      ttbThumbTics:
        begin
          Part := TKP_TICS;
          //Base := Ord(ttbThumbTics);
        end;
      ttbThumbTicsVert:
        begin
          Part := TKP_TICSVERT;
          //Base := Ord(ttbThumbTicsVert);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

function TThemeServices.GetElementDetails(Detail: TThemedTrayNotify): TThemedElementDetails;
{var
  Base: Integer;}
begin
{  Result.Element := teTrayNotify;
  with Result do
  begin
    case Detail of
      ttnBackground:
        begin
          Part := TNP_BACKGROUND;
          Base := Ord(ttnBackground);
        end;
      ttnAnimBackground:
        begin
          Part := TNP_ANIMBACKGROUND;
          Base := Ord(ttnAnimBackground);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;}
  NotImplemented;
end;

function TThemeServices.GetElementDetails(Detail: TThemedTreeview): TThemedElementDetails;
{var
  Base: Integer;}
begin
{  Result.Element := teTreeView;
  with Result do
  begin
    case Detail of
      ttItemNormal..ttItemSelectedNotFocus:
        begin
          Part := TVP_TREEITEM;
          Base := Ord(ttItemNormal);
        end;
      ttGlyphClosed..ttGlyphOpened:
        begin
          Part := TVP_GLYPH;
          Base := Ord(ttGlyphClosed);
        end;
      ttBranch:
        begin
          Part := TVP_BRANCH;
          Base := Ord(ttBranch);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;}
  NotImplemented;
end;

function TThemeServices.GetElementDetails(Detail: TThemedWindow): TThemedElementDetails;
{var
  Base: Integer;}
begin
{  Result.Element := teWindow;
  with Result do
  begin
    case Detail of
      twCaptionActive..twCaptionDisabled:
        begin
          Part := WP_CAPTION;
          Base := Ord(twCaptionActive);
        end;
      twSmallCaptionActive..twSmallCaptionDisabled:
        begin
          Part := WP_SMALLCAPTION;
          Base := Ord(twSmallCaptionActive);
        end;
      twMinCaptionActive..twMinCaptionDisabled:
        begin
          Part := WP_MINCAPTION;
          Base := Ord(twMinCaptionActive);
        end;
      twSmallMinCaptionActive..twSmallMinCaptionDisabled:
        begin
          Part := WP_SMALLMINCAPTION;
          Base := Ord(twSmallMinCaptionActive);
        end;
      twMaxCaptionActive..twMaxCaptionDisabled:
        begin
          Part := WP_MAXCAPTION;
          Base := Ord(twMaxCaptionActive);
        end;
      twSmallMaxCaptionActive..twSmallMaxCaptionDisabled:
        begin
          Part := WP_SMALLMAXCAPTION;
          Base := Ord(twSmallMaxCaptionActive);
        end;
      twFrameLeftActive..twFrameLeftInactive:
        begin
          Part := WP_FRAMELEFT;
          Base := Ord(twFrameLeftActive);
        end;
      twFrameRightActive..twFrameRightInactive:
        begin
          Part := WP_FRAMERIGHT;
          Base := Ord(twFrameRightActive);
        end;
      twFrameBottomActive..twFrameBottomInactive:
        begin
          Part := WP_FRAMEBOTTOM;
          Base := Ord(twFrameBottomActive);
        end;
      twSmallFrameLeftActive..twSmallFrameLeftInactive:
        begin
          Part := WP_SMALLFRAMELEFT;
          Base := Ord(twSmallFrameLeftActive);
        end;
      twSmallFrameRightActive..twSmallFrameRightInactive:
        begin
          Part := WP_SMALLFRAMERIGHT;
          Base := Ord(twSmallFrameRightActive);
        end;
      twSmallFrameBottomActive..twSmallFrameBottomInactive:
        begin
          Part := WP_SMALLFRAMEBOTTOM;
          Base := Ord(twSmallFrameBottomActive);
        end;
      twSysButtonNormal..twSysButtonDisabled:
        begin
          Part := WP_SYSBUTTON;
          Base := Ord(twSysButtonNormal);
        end;
      twMDISysButtonNormal..twMDISysButtonDisabled:
        begin
          Part := WP_MDISYSBUTTON;
          Base := Ord(twMDISysButtonNormal);
        end;
      twMinButtonNormal..twMinButtonDisabled:
        begin
          Part := WP_MINBUTTON;
          Base := Ord(twMinButtonNormal);
        end;
      twMDIMinButtonNormal..twMDIMinButtonDisabled:
        begin
          Part := WP_MDIMINBUTTON;
          Base := Ord(twMDIMinButtonNormal);
        end;
      twMaxButtonNormal..twMaxButtonDisabled:
        begin
          Part := WP_MAXBUTTON;
          Base := Ord(twMaxButtonNormal);
        end;
      twCloseButtonNormal..twCloseButtonDisabled:
        begin
          Part := WP_CLOSEBUTTON;
          Base := Ord(twCloseButtonNormal);
        end;
      twSmallCloseButtonNormal..twSmallCloseButtonDisabled:
        begin
          Part := WP_SMALLCLOSEBUTTON;
          Base := Ord(twSmallCloseButtonNormal);
        end;
      twMDICloseButtonNormal..twMDICloseButtonDisabled:
        begin
          Part := WP_MDICLOSEBUTTON;
          Base := Ord(twMDICloseButtonNormal);
        end;
      twRestoreButtonNormal..twRestoreButtonDisabled:
        begin
          Part := WP_RESTOREBUTTON;
          Base := Ord(twRestoreButtonNormal);
        end;
      twMDIRestoreButtonNormal..twMDIRestoreButtonDisabled:
        begin
          Part := WP_MDIRESTOREBUTTON;
          Base := Ord(twMDIRestoreButtonNormal);
        end;
      twHelpButtonNormal..twHelpButtonDisabled:
        begin
          Part := WP_HELPBUTTON;
          Base := Ord(twHelpButtonNormal);
        end;
      twMDIHelpButtonNormal..twMDIHelpButtonDisabled:
        begin
          Part := WP_MDIHELPBUTTON;
          Base := Ord(twMDIHelpButtonNormal);
        end;
      twHorzScrollNormal..twHorzScrollDisabled:
        begin
          Part := WP_HORZSCROLL;
          Base := Ord(twHorzScrollNormal);
        end;
      twHorzThumbNormal..twHorzThumbDisabled:
        begin
          Part := WP_HORZTHUMB;
          Base := Ord(twHorzThumbNormal);
        end;
      twVertScrollNormal..twVertScrollDisabled:
        begin
          Part := WP_VERTSCROLL;
          Base := Ord(twVertScrollNormal);
        end;
      twVertThumbNormal..twVertThumbDisabled:
        begin
          Part := WP_VERTTHUMB;
          Base := Ord(twVertThumbNormal);
        end;
      twDialog:
        begin
          Part := WP_DIALOG;
          Base := Ord(twDialog);
        end;
      twCaptionSizingTemplate:
        begin
          Part := WP_CAPTIONSIZINGTEMPLATE;
          Base := Ord(twCaptionSizingTemplate);
        end;
      twSmallCaptionSizingTemplate:
        begin
          Part := WP_SMALLCAPTIONSIZINGTEMPLATE;
          Base := Ord(twSmallCaptionSizingTemplate);
        end;
      twFrameLeftSizingTemplate:
        begin
          Part := WP_FRAMELEFTSIZINGTEMPLATE;
          Base := Ord(twFrameLeftSizingTemplate);
        end;
      twSmallFrameLeftSizingTemplate:
        begin
          Part := WP_SMALLFRAMELEFTSIZINGTEMPLATE;
          Base := Ord(twSmallFrameLeftSizingTemplate);
        end;
      twFrameRightSizingTemplate:
        begin
          Part := WP_FRAMERIGHTSIZINGTEMPLATE;
          Base := Ord(twFrameRightSizingTemplate);
        end;
      twSmallFrameRightSizingTemplate:
        begin
          Part := WP_SMALLFRAMERIGHTSIZINGTEMPLATE;
          Base := Ord(twSmallFrameRightSizingTemplate);
        end;
      twFrameBottomSizingTemplate:
        begin
          Part := WP_FRAMEBOTTOMSIZINGTEMPLATE;
          Base := Ord(twFrameBottomSizingTemplate);
        end;
      twSmallFrameBottomSizingTemplate:
        begin
          Part := WP_SMALLFRAMEBOTTOMSIZINGTEMPLATE;
          Base := Ord(twSmallFrameBottomSizingTemplate);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;}
  NotImplemented;
end;

function TThemeServices.ContentRect(DC: QPainterH; Details: TThemedElementDetails; BoundingRect: TRect): TRect;
begin
  with Details do
    GetThemeBackgroundContentRect(Element, DC, Part, State, BoundingRect, @Result);
end;

function TThemeServices.ContentRect(Canvas: TCanvas; Details: TThemedElementDetails; BoundingRect: TRect): TRect;
begin
  Canvas.Start;
  try
    Result := ContentRect(Canvas.Handle, Details, BoundingRect);
  finally
    Canvas.Stop;
  end;
end;

procedure TThemeServices.DrawEdge(DC: QPainterH; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal;
  ContentRect: PRect = nil);
begin
  NotImplemented;
{  with Details do
    DrawThemeEdge(Theme[Element], HDC(QPainter_handle(DC)), Part, State,
      R, Edge, Flags,
      ContentRect);}
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeServices.DrawEdge(Canvas: TCanvas; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal;
  ContentRect: PRect = nil);
begin
  Canvas.Start;                                               
  try
    DrawEdge(Canvas.Handle, Details, R, Edge, Flags, ContentRect);
  finally
    Canvas.Stop;
  end;
end;

procedure TThemeServices.DrawElement(DC: QPainterH; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect = nil);
begin
  with Details do
    DrawThemeBackground(Element, DC, Part, State, R, ClipRect);
end;

procedure TThemeServices.DrawElement(Canvas: TCanvas; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect = nil);
begin
  Canvas.Start;
  try
    DrawElement(Canvas.Handle, Details, R, ClipRect);
  finally
    Canvas.Stop;
  end;
end;

procedure TThemeServices.DrawParentBackground(Window: QWidgetH; Target: QPainterH; Details: PThemedElementDetails;
  OnlyIfTransparent: Boolean; Bounds: PRect = nil);
var
  DoDraw: Boolean;
  ABounds: TRect;
  Control: TWidgetControl;
  Widget: TWidgetControl;
  Pt: TPoint;
begin
  if OnlyIfTransparent and Assigned(Details) then
  begin
    with Details^ do
      DoDraw := IsThemeBackgroundPartiallyTransparent(Element, Part, State);
  end
  else
    DoDraw := True;
  if DoDraw then
  begin
    Control := FindControl(Window);
    if Control <> nil then
    begin
      Widget := Control.Parent;
      while (Widget <> nil) and (TOpenWidgetControl(Widget).Masked) do
        Widget := Widget.Parent;

      if (Widget <> nil) and (Widget is TTabSheet) then
      begin
        if (TTabSheet(Widget).PageControl <> nil) and
           (TTabSheet(Widget).PageControl.Style = tsTabs) then
        begin
          ABounds := TTabSheet(Widget).ClientRect;
          Pt := Control.ClientToScreen(Point(0, 0));
          Pt := Widget.ScreenToClient(Pt);
          OffsetRect(ABounds, -Pt.X, -Pt.Y);
          ThemeServices.DrawElement(Target, GetElementDetails(ttBody), ABounds, Bounds);
          Exit;
        end;
      end;
    end;
    DrawThemeParentBackground(Window, Target, Bounds);
  end;
end;

procedure TThemeServices.DrawParentBackground(Window: QWidgetH; Target: TCanvas; Details: PThemedElementDetails; OnlyIfTransparent: Boolean;
  Bounds: PRect = nil);
begin
  Target.Start;
  try
    DrawParentBackground(Window, Target.Handle, Details, OnlyIfTransparent, Bounds);
  finally
    Target.Stop;
  end;
end;

procedure TThemeServices.DrawText(DC: QPainterH; Details: TThemedElementDetails; const S: WideString; R: TRect; Flags,
  Flags2: Cardinal);
begin
  with Details do
    DrawThemeText(Element, DC, Part, State, S, Flags, Flags2, R);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeServices.DrawText(Canvas: TCanvas; Details: TThemedElementDetails; const S: WideString; R: TRect; Flags,
  Flags2: Cardinal);
begin
  with Details do
    DrawThemeText(Element, Canvas.Handle, Part, State, S, Flags, Flags2, R);
end;

function TThemeServices.HasTransparentParts(Details: TThemedElementDetails): Boolean;
begin
  with Details do
    Result := IsThemeBackgroundPartiallyTransparent(Element, Part, State);
end;

type
  TOpenFrameControl = class(TFrameControl);

procedure TThemeServices.PaintBorder(Control: TWinControl; EraseLRCorner: Boolean; Painter: QPainterH = nil);
var
  EmptyRect,
  DrawRect: TRect;
  Details: TThemedElementDetails;
  DC: QPainterH;
begin
  with Control do
  begin
    if (Control is TFrameControl) and not (Control is TCustomForm) and
       (TOpenFrameControl(Control).BorderStyle in [bsSingle, bsSunken3d]) then
    begin
      DrawRect := Control.BoundsRect;
      OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
      if Painter <> nil then
        DC := Painter
      else
      begin
        DC := QPainter_create;
        QPainter_begin(DC, TOpenFrameControl(Control).GetPaintDevice);
      end;
      try
        EmptyRect := DrawRect;
        if EraseLRCorner then
        begin
          {AStyle := GetWindowLong(HWND(QWidget_winId(Handle)), GWL_STYLE);
          if ((AStyle and WS_HSCROLL) <> 0) and ((AStyle and WS_VSCROLL) <> 0) then
          begin
            W := GetSystemMetrics(SM_CXVSCROLL);
            H := GetSystemMetrics(SM_CYHSCROLL);
            InflateRect(EmptyRect, -2, -2);
            with EmptyRect do
              EmptyRect := Rect(Right - W, Bottom - H, Right, Bottom);
            FillRect(DC, EmptyRect, GetSysColorBrush(COLOR_BTNFACE));
          end;}
        end;
        QPainter_save(DC);
        try
          QPainter_setClipRect(DC, @DrawRect); // ignore old clip region
          with DrawRect do
            QExcludeClipRect(DC, Left + 2, Top + 2, Right - 2, Bottom - 2);
          Details := GetElementDetails(teEditTextNormal);
          DrawElement(DC, Details, DrawRect);
        finally
          QPainter_restore(DC);
        end;
      finally
        if Painter = nil then
        begin
          QPainter_end(DC);
          QPainter_destroy(DC);
        end;
      end;
    end;
  end;
end;


{ TPainterCanvas }

procedure TPainterCanvas.BeginPainting;
begin
  // do nothing
end;

constructor TPainterCanvas.Create(AHandle: QPainterH);
begin
  inherited Create;
  Handle := AHandle;
  Start(False);
end;

destructor TPainterCanvas.Destroy;
begin
  Stop;
  inherited Destroy;
end;

initialization

finalization
  InternalThemeServices.Free;

end.
