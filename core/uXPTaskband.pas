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
unit uXPTaskband;

interface

uses
    Classes, QExtCtrls, uCommon,
    QControls, uXPPNG, QGraphics,
    uWMConsts, XLib, Types, Sysutils;

type
    TXPTask=class;

    { TODO : Add group behaviour }
    TXPTaskband=class(TPanel)
    private
        activetasks: TList;
        FNormalTask: TBitmap;

        FOriginalOver: TBitmap;
        FOverTask: TBitmap;

        FOriginalPressed: TBitmap;
        FPressedTask: TBitmap;
    public
        procedure resizeBitmaps(const width:integer);
        procedure updatetaskswidth;
        procedure releasetasks;
        procedure updatetask(const client:IWMClient);
        procedure activatetask(const client:IWMClient);
        procedure addtask(const client:IWMClient);
        procedure removetask(const client:IWMClient);
        procedure setup;
        constructor Create(AOwner:TComponent);override;
        destructor Destroy;override;
    end;

    TXPTask=class(TPanel)
    private
        FIcon:TBitmap;
        FTaskName: string;
        FDown: boolean;
        procedure SetTaskName(const Value: string);
        procedure SetDown(const Value: boolean);
    public
        window: TWindow;
        procedure mouseenter(AControl:TControl);override;
        procedure mouseleave(AControl:TControl);override;
        procedure MouseDown(button: TMouseButton; Shift: TShiftState; X, Y: integer);override;
        procedure MouseUp(button: TMouseButton; Shift: TShiftState; X, Y: integer);override;
        procedure paint;override;
        procedure setup;
        constructor Create(AOwner:TComponent);override;
        destructor Destroy;override;
        property TaskName:string read FTaskName write SetTaskName;
        property Icon: TBitmap read FIcon write FIcon;
        property Down: boolean read FDown write SetDown;
    end;

implementation

uses uWindowManager;

{ TXPTaskband }

procedure TXPTaskband.activatetask(const client: IWMClient);
var
    task: TXPTask;
    btask: TXPTask;
    i:integer;
    w: TWindow;
    k: integer;
begin
    if assigned(client) then begin
        w:=client.getwindow;
        for i := 0 to activetasks.count-1 do begin
            task:=activetasks[i];
            if task.window=w then begin
                for k := 0 to activetasks.count-1 do begin
                    btask:=activetasks[k];
                    if (btask<>task) then btask.down:=false;
                end;
                task.down:=true;
                break;
            end;
        end;
    end
    else begin
        releaseTasks;
    end;
end;

procedure TXPTaskband.addtask(const client: IWMClient);
var
    task: TXPTask;
    found: boolean;
    i:integer;
    w: TWindow;
begin
    w:=client.getwindow;
    found:=false;
    for i:=activetasks.count-1 downto 0 do begin
        task:=activetasks[i];
        if (task.window=w) then begin
            found:=true;
            break;
            task.taskname:=client.getTitle;
            task.Hint:=client.getTitle;
            task.PopupMenu:=client.getSystemMenu;
        end;
    end;
    if not found then begin
        task:=TXPTask.create(nil);
        task.window:=w;
        task.TaskName:=client.getTitle;
        task.Hint:=client.getTitle;
        task.PopupMenu:=client.getSystemMenu;
        //t.OnMouseUp:=toolbutton1mouseup;
        task.Icon.Assign(client.getbitmap);
        activetasks.add(task);
        updatetaskswidth;
        task.Left:=clientwidth;
        task.Parent:=self;
    end;
end;

constructor TXPTaskband.Create(AOwner: TComponent);
begin
  inherited;
  FNormalTask:=TBitmap.create;

  FOriginalOver:=TBitmap.create;
  FOverTask:=TBitmap.create;

  FOriginalPressed:=TBitmap.create;
  FPressedTask:=TBitmap.create;

  activetasks:=TList.create;
  setup;
end;

destructor TXPTaskband.Destroy;
begin
  FOriginalPressed.free;
  FPressedTask.free;

  FOriginalOver.free;
  FOverTask.free;
  FNormalTask.free;
  activetasks.free;
  inherited;
end;

procedure TXPTaskband.releasetasks;
var
    task: TXPTask;
    i:integer;
begin
    for i := 0 to activetasks.count-1 do begin
        task:=activetasks[i];
        task.down:=false;
    end;
end;

procedure TXPTaskband.removetask(const client: IWMClient);
var
    task: TXPTask;
    i:integer;
    w: TWindow;
begin
    w:=client.getwindow;
    for i := 0 to activetasks.count - 1 do begin
        task:=activetasks[I];
        if (task.window=w) then begin
            activetasks.remove(task);
            task.free;
            updatetaskswidth;
            break;
        end;
    end;
end;

procedure TXPTaskband.resizeBitmaps(const width: integer);
var
    temp:TBitmap;
begin
{ TODO : Take how to resize backgrounds from the theme instead do it hardcoded }
   FOverTask.Width:=width;
   FOverTask.Canvas.CopyRect(rect(0,0,3,FOverTask.height),FOriginalOver.canvas,rect(0,0,3,FOverTask.height));
   FOverTask.Canvas.CopyRect(rect(width-3,0,width,FOverTask.height),FOriginalOver.canvas,rect(FOriginalOver.width-3,0,FOriginalOver.width,FOverTask.height));

   FPressedTask.Width:=width;
   FPressedTask.Canvas.CopyRect(rect(0,0,3,FPressedTask.height),FOriginalPressed.canvas,rect(0,0,3,FPressedTask.height));
   FPressedTask.Canvas.CopyRect(rect(width-3,0,width,FPressedTask.height),FOriginalPressed.canvas,rect(FOriginalPressed.width-3,0,FOriginalPressed.width,FPressedTask.height));

   temp:=TBitmap.create;
   try
       temp.width:=FOriginalOver.Width-8;
       temp.height:=FOverTask.height;
       temp.Canvas.copyrect(rect(0,0,FOriginalOver.width-6,FOverTask.height),FOriginalOver.canvas,rect(4,0,FOriginalOver.width-4,FOriginalOver.height));
       FOverTask.canvas.stretchdraw(rect(4,0,width-3,FOverTask.height),temp);

       temp.width:=FOriginalPressed.Width-8;
       temp.height:=FPressedTask.height;
       temp.Canvas.copyrect(rect(0,0,FOriginalPressed.width-6,FPressedTask.height),FOriginalPressed.canvas,rect(4,0,FOriginalPressed.width-4,FOriginalPressed.height));
       FPressedTask.canvas.stretchdraw(rect(4,0,width-3,FPressedTask.height),temp);
   finally
        temp.Free;
   end
end;

procedure TXPTaskband.setup;
var
    dir: string;
begin
    BevelOuter:=bvNone;
    dir:=getSystemInfo(XP_TASKBAR_DIR);
    { TODO : Allow any width or height, tasks must organize depending on the dimension controls }
    bitmap.LoadFromFile(dir+'/taskbar_background_bottom.png');
    FNormalTask.LoadFromFile(dir+'/taskband_button_normal.png');

    FOverTask.LoadFromFile(dir+'/taskband_button_over.png');
    FOriginalOver.Assign(FOverTask);

    FPressedTask.LoadFromFile(dir+'/taskband_button_press.png');
    FOriginalPressed.Assign(FPressedTask);
end;

procedure TXPTaskband.updatetask(const client: IWMClient);
var
    task: TXPTask;
    i:integer;
    w: TWindow;
begin
    w:=client.getwindow;
    for i := 0 to activetasks.count - 1 do begin
        task:=activetasks[i];
        if (task.window=w) then begin
            task.TaskName:=client.getTitle;
            break;
        end;
    end;
end;

procedure TXPTaskband.updatetaskswidth;
var
    bw: integer;
    i: integer;
    task: TXPTask;
begin
    if activetasks.count>0 then begin
        bw:=trunc((clientWidth-2) / activetasks.count);
        if bw>163 then bw:=163;

        for i := 0 to activetasks.count - 1 do begin
            task:=activetasks[I];
            task.width:=bw;
        end;
        resizeBitmaps(bw);
    end;
end;

{ TXPTask }

constructor TXPTask.Create(AOwner: TComponent);
begin
  inherited;
  FDown:=false;
  FTaskName:='(no name)';
  FIcon:=TBitmap.create;
  setup;
end;

destructor TXPTask.Destroy;
begin
  FIcon.free;
  inherited;
end;

procedure TXPTask.MouseDown(button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  bitmap.Assign((parent as TXPTaskBand).FPressedTask);
end;

procedure TXPTask.mouseenter(AControl: TControl);
begin
  inherited;
  bitmap.Assign((parent as TXPTaskBand).FOverTask);
end;

procedure TXPTask.mouseleave(AControl: TControl);
begin
  inherited;
  if (FDown) then bitmap.Assign((parent as TXPTaskBand).FPressedTask)
  else bitmap.Assign((parent as TXPTaskBand).FNormalTask);
end;

procedure TXPTask.MouseUp(button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
var
    c: TWMClient;
begin
  inherited;
  c:=XPWindowManager.findClient(window);
  if assigned(c) then c.activate;

  if ptinrect(clientrect,point(x,y)) then bitmap.assign((parent as TXPTaskBand).FOverTask)
  else begin
    if (FDown) then bitmap.Assign((parent as TXPTaskBand).FPressedTask)
    else bitmap.Assign((parent as TXPTaskBand).FNormalTask);
  end;
end;

procedure TXPTask.paint;
var
    text: string;
    k: integer;
begin
  inherited;
  text:=FTaskName;
  FIcon.transparent:=true;
  Canvas.Draw(11,((height-FIcon.Height) div 2)+2,FIcon);
  canvas.font.color:=clWhite;
  k:=0;
  while (canvas.TextWidth(text)>width-16-FIcon.width) do begin
    text:=copy(FTaskName,1,length(FTaskName)-k)+'...';
    inc(k);
  end;
  Canvas.TextOut(FIcon.width+5+11,((height-Canvas.textheight(text)) div 2)+1,text);
end;

procedure TXPTask.SetDown(const Value: boolean);
begin
    if (FDown<>Value) then begin
        FDown := Value;
        if (FDown) then bitmap.Assign((parent as TXPTaskBand).FPressedTask)
        else bitmap.Assign((parent as TXPTaskBand).FNormalTask);
        invalidate;
    end;
end;

procedure TXPTask.SetTaskName(const Value: string);
begin
    if (Value<>FTaskName) then begin
        FTaskName := Value;
        invalidate;
    end;
end;

procedure TXPTask.setup;
var
    dir: string;
begin
    BevelOuter:=bvNone;
    dir:=getSystemInfo(XP_TASKBAR_DIR);
    bitmap.LoadFromFile(dir+'/taskband_button_normal.png');
    FIcon.LoadFromFile(dir+'/no_icon.png');
    width:=163; //Default task width
    align:=alLeft;
end;

end.
