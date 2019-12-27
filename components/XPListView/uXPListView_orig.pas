unit uXPListView_orig;

interface

{ TODO : Add more functionality }

uses Classes, Types, QControls, QStdCtrls,
     QGraphics, QExtCtrls, uGraphics,
     Qt, uXPImageList, QForms, QFileCtrls, Sysutils;

type
    TXPListView=class;

    TXPListItem=class(TObject)
    private
        FCaption: string;
        FImageIndex: integer;
        FColumnData: TStrings;
        procedure SetCaption(const Value: string);
        procedure SetImageIndex(const Value: integer);
        procedure SetColumnData(const Value: TStrings);
    public
        constructor Create; virtual;
        destructor Destroy;override;
        property Caption:string read FCaption write SetCaption;
        property ImageIndex:integer read FImageIndex write SetImageIndex;
        property ColumnData: TStrings read FColumnData write SetColumnData;
    end;

    TXPListItems=class(TObject)
    private
        FItems: TList;
        FOwner: TXPListView;
        procedure destroyItems;
    function GetCount: integer;
    public
        function Add: TXPListItem;
        function Insert(const Index: integer): TXPListItem;
        constructor Create(AOwner:TXPListView); virtual;
        destructor Destroy;override;
        property Items: TList read FItems;
        property Count: integer read GetCount;
    end;

    TXPListViewGetImageIndexEvent=function (Sender:TObject; const itemindex:integer): integer of object;


    TXPListView=class(TCustomControl)
    private
        FBorderStyle: TBorderStyle;
        FBorderWidth: integer;
        FImageList: TXPImageList;
        FItems: TXPListItems;
        FIconHSpacing: integer;
        FIconVSpacing: integer;
        FRowHeight: integer;
        FColWidth: integer;
        FIconsPerRow: integer;
        FVertScrollBar: TScrollBar;
        FItemCount: integer;
        FRows: integer;
        FBuffer: TBitmap;
        FOnGetImageIndex: TXPListViewGetImageIndexEvent;
        procedure SetBorderStyle(const Value: TBorderStyle);
        procedure SetBorderWidth(const Value: integer);
        procedure SetImageList(const Value: TXPImageList);
        procedure SetIconHSpacing(const Value: integer);
        procedure SetIconVSpacing(const Value: integer);
        procedure SetItemCount(const Value: integer);
        procedure SetOnGetImageIndex(const Value: TXPListViewGetImageIndexEvent);
        procedure InternalCalc;
        procedure RecreateBuffer;
    protected
        function WidgetFlags: Integer; override;
        procedure BorderStyleChanged; dynamic;
        function GetClientOrigin: TPoint; override;
        function GetClientRect: TRect; override;
        procedure InvalidateEvent(Sender: TObject);
    public
        procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);override;
        procedure paint;override;
        property Bitmap;
        constructor Create(AOwner:TComponent);override;
        destructor Destroy;override;
    published
        property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
        property BorderWidth: integer read FBorderWidth write SetBorderWidth;
        property ImageList: TXPImageList read FImageList write SetImageList;
        property Items: TXPListItems read FItems;
        property ItemCount: integer read FItemCount write SetItemCount;
        property IconHSpacing: integer read FIconHSpacing write SetIconHSpacing;
        property IconVSpacing: integer read FIconVSpacing write SetIconVSpacing;
        property VertScrollBar: TScrollBar read FVertScrollBar;
        property OnGetImageIndex: TXPListViewGetImageIndexEvent read FOnGetImageIndex write SetOnGetImageIndex;
    end;

implementation

{ TXPListItem }

constructor TXPListItem.Create;
begin
    FColumnData:=TStringList.create;
    FImageIndex:=-1;
    FCaption:='';
end;

destructor TXPListItem.Destroy;
begin
    FColumnData.free;
    inherited;
end;

procedure TXPListItem.SetCaption(const Value: string);
begin
    if Value<>FCaption then begin
        FCaption := Value;
    end;
end;

procedure TXPListItem.SetColumnData(const Value: TStrings);
begin
    if Value<>FColumnData then begin
        FColumnData.assign(Value);
    end;
end;

procedure TXPListItem.SetImageIndex(const Value: integer);
begin
    if Value<>FImageIndex then begin
        FImageIndex := Value;
    end;
end;

{ TXPListItems }

function TXPListItems.Add: TXPListItem;
begin
    result:=TXPListItem.create;
    FItems.add(result);
end;

constructor TXPListItems.Create(AOwner:TXPListView);
begin
    FItems:=TList.create;
    FOwner:=AOwner;
end;

destructor TXPListItems.Destroy;
begin
    destroyItems;
    FItems.free;
    inherited;
end;

procedure TXPListItems.destroyItems;
var
    i:longint;
    item: TXPListItem;
begin
    for i:=FItems.count-1 downto 0 do begin
        item:=FItems[i];
        item.free;
    end;
end;

function TXPListItems.GetCount: integer;
begin
    result:=fitems.Count;
end;

function TXPListItems.Insert(const Index: integer): TXPListItem;
begin
    result:=TXPListItem.create;
    FItems.Insert(Index,result);
end;

{ TXPListView }

procedure TXPListView.BorderStyleChanged;
begin
    //Notification
end;

constructor TXPListView.Create(AOwner: TComponent);
begin
    inherited;
    FBuffer:=TBitmap.create;
    FOnGetImageIndex:=nil;
    IconHSpacing:=43;
    IconVSpacing:=43;
    FItems:=TXPListItems.Create(self);
    FImageList:=nil;
    FBorderStyle:=bsSunken3D;
    FBorderWidth:=1;
    FVertScrollBar:=TScrollBar.create(nil);
    FVertScrollBar.Kind:=sbVertical;
    FVertScrollBar.OnChange:=InvalidateEvent;
    color:=clWindow;
end;

destructor TXPListView.Destroy;
begin
  FBuffer.free;
  FVertScrollBar.free;
  FItems.free;
  inherited;
end;

function TXPListView.GetClientOrigin: TPoint;
begin
  Result.X := FBorderWidth*2;
  Result.Y := Result.X;
  QWidget_mapToGlobal(Handle, @Result, @Result);
end;

function TXPListView.GetClientRect: TRect;
var
  FW: Integer;
begin
  Result := inherited GetClientRect;
  FW := FBorderWidth*2;
  InflateRect(Result, -FW, -FW);
end;

procedure TXPListView.InternalCalc;
begin
  FIconsPerRow:=(clientwidth div FColWidth);
  FRows:=FItemCount div FIConsPerRow;
  FVertScrollBar.Max:=FRows*FRowHeight;
  FVertScrollBar.SmallChange:=1;
  FVertScrollBar.LargeChange:=FRowHeight;  
end;

procedure TXPListView.InvalidateEvent(Sender: TObject);
begin
    invalidate;
end;

procedure TXPListView.paint;
const
  cColor: array [Boolean] of Integer = (clBtnShadow, clBtnHighlight);
var
  aRect: TRect;
  dRect: TRect;
  x,y: integer;
  i: integer;
  imageIndex: integer;
  dtop: integer;
  rownum: integer;
begin
    exit;
    aRect:=Rect(0,0,width,height);
    draw3DRect(Canvas, aRect, FBorderStyle);
    dRect:=clientrect;

    canvas.SetClipRect(dRect);
    y:=dRect.top-(fvertscrollbar.Position);
    rownum:=abs(y) div FRowHeight;
    i:=rownum*FIconsPerRow;
    dtop:=rownum*FRowHeight;
    y:=y+dtop;

//    application.mainform.caption:=' i '+inttostr(i)+' rownum:'+inttostr(rownum)+' iconsperrow:'+inttostr(FIconsPerRow);
    if (FItemCount>=1) then begin
        while y<dRect.Bottom do begin
            x:=dRect.left+1;
            while x<dRect.right do begin
                if (i<FItemCount) and (x+fcolwidth<dRect.right) then begin
                    if assigned(FImageList) then begin
                        if assigned(FOnGetImageIndex) then imageIndex:=FOnGetImageIndex(self,i)
                        else imageIndex:=-1;
                        fbuffer.canvas.CopyRect(Rect(0,0,FColWidth,FRowHeight), bitmap.canvas, rect(x,y,x+fcolwidth,y+frowheight));
                        if imageindex<>-1 then begin
                            FImageList.Draw(fbuffer.canvas,(FIconHSpacing div 2),1,imageIndex);
                            canvas.Draw(x,y,fbuffer);
                        end
                        else begin
                            canvas.Draw(x,y,fbuffer);
                        end;
                    end;
                    inc(i);
                end
                else begin
                    fbuffer.canvas.CopyRect(Rect(0,0,FColWidth,FRowHeight), bitmap.canvas, rect(x,y,x+fcolwidth,y+frowheight));
                    canvas.Draw(x,y,fbuffer);
                    //InvalidateRect(rect(x,y,x+fcolwidth,y+frowheight),true);
                end;
                inc(x,FColWidth);
                //if (i>FItemCount) then exit;
            end;
            inc(y,FRowHeight);
        end;
    end;
end;

procedure TXPListView.RecreateBuffer;
begin
    fbuffer.Width:=FColWidth;
    fbuffer.Height:=FRowHeight;
end;

procedure TXPListView.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then begin
    FBorderStyle := Value;
    Invalidate;
    BorderStyleChanged;
  end;
end;

procedure TXPListView.SetBorderWidth(const Value: integer);
begin
    if (Value<>FBorderWidth) then begin
        FBorderWidth := Value;
        invalidate;
    end;
end;

procedure TXPListView.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  FVertScrollBar.parentcolor:=false;
  if FVertScrollbar.parent<>self then FVertScrollBar.parent:=self;
  FVertScrollBar.Align:=alRight;
  InternalCalc;
end;

procedure TXPListView.SetIconHSpacing(const Value: integer);
begin
    if value<>FIconHSpacing then begin
        FIconHSpacing := Value;
        FColWidth:=FIconHSpacing+32;
        recreateBuffer;
        invalidate;
    end;
end;

procedure TXPListView.SetIconVSpacing(const Value: integer);
begin
    if value<>FIconVSpacing then begin
        FIconVSpacing := Value;
        FRowHeight:=FIconVSpacing+32;
        recreateBuffer;        
        invalidate;
    end;
end;

procedure TXPListView.SetImageList(const Value: TXPImageList);
begin
    if value<>FImageList then begin
        FImageList := Value;
    end;
end;



procedure TXPListView.SetItemCount(const Value: integer);
begin
    if value<>FItemCount then begin
        FItemCount := Value;
        InternalCalc;
        invalidate;
    end;
end;

procedure TXPListView.SetOnGetImageIndex(const Value: TXPListViewGetImageIndexEvent);
begin
    FOnGetImageIndex := Value;
end;

function TXPListView.WidgetFlags: Integer;
begin
    Result := Inherited WidgetFlags or Integer(WidgetFlags_WRepaintNoErase) or Integer(WidgetFlags_WResizeNoErase);
end;

end.
