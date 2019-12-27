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
unit uActiveTasks;

interface

uses
  SysUtils, Types, Classes,XLib,
  Variants, QTypes, QGraphics, Qt,
  QControls, QForms, uGraphics, 
  QDialogs, QStdCtrls, QExtCtrls;

type
  TActiveTasksDlg = class(TForm)
    Label1: TLabel;
    Timer1: TTimer;
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    activetask: integer;
    function getDottedText(const text: string; maxwidth: integer): string;
  public
    { Public declarations }
    procedure incActiveTask;
  end;

var
  ActiveTasksDlg: TActiveTasksDlg=nil;

implementation

uses uWindowManager;

{$R *.xfm}

procedure TActiveTasksDlg.FormPaint(Sender: TObject);
var
    x,y: integer;
    w: integer;
    c: integer;
    cli: TWMClient;
    b: TBitmap;
    nc: integer;
    ox: integer;
begin
        y:=19;
        nc:=xpwindowmanager.framedclients.count;
        if (nc>7) then nc:=7;

        w:=nc*43;
        ox:=(width-w) div 2;

        x:=ox;

        canvas.pen.width:=1;
        R3D(canvas,clientrect);
        R3D(canvas,rect(14,height-35,315,height-10),false,false,true);

        c:=0;
        canvas.pen.color:=clHighlight;
        canvas.pen.width:=2;
        while (c<xpwindowmanager.framedclients.count) do begin
            cli:=xpwindowmanager.framedclients[c];
            if c=activetask then begin
                canvas.rectangle(rect(x,y,x+43,y+43));
            end;

            b:=cli.geticon;
            if assigned(b) then begin
                b.transparent:=true;
                canvas.draw(x+5,y+5,b);
                x:=x+43;

                if ((c+1) mod 7)=0 then begin
                    x:=ox;
                    y:=y+43;
                end;
            end;
            inc(c);
        end;
end;

procedure TActiveTasksDlg.FormShow(Sender: TObject);
var
    rows: integer;
begin
    width:=330;
    height:=107;
    rows:=((xpwindowmanager.framedclients.count-1) div 7)+1;
    height:=height+(43*(rows-1));
    activetask:=0;
    incActiveTask;
    left:=(screen.Width-width) div 2;
    top:=((screen.height-height) div 2)-107;
end;

procedure TActiveTasksDlg.Timer1Timer(Sender: TObject);
var
    keymap: TXQueryKeymap;
    c: TWMClient;
begin
    //This is shit!
    xquerykeymap(qtdisplay,keymap);

    if keymap[8]=#0 then begin
        timer1.enabled:=false;
        c:=xpwindowmanager.framedclients[activetask];
        if assigned(c) then begin
            c.activate;
            activetasksdlg:=nil;
            close;
        end;
    end;
end;

procedure TActiveTasksDlg.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
    action:=caFree;
end;

function TActiveTasksDlg.getDottedText(const text:string;maxwidth:integer):string;
var
    w: integer;
    wc: widestring;
    k: integer;
begin
    wc:=text;

    k:=1;
    canvas.Font.assign(label1.font);
    w:=canvas.TextWidth(wc);

    while w>maxwidth do begin
        wc:=copy(text,1,length(text)-k)+'...';
        w:=canvas.TextWidth(wc);
        inc(k);
    end;

    result:=wc;
end;

procedure TActiveTasksDlg.incActiveTask;
var
    c: TWMClient;
begin
    inc(activetask);
    if activetask>=xpwindowmanager.framedclients.count then begin
        activetask:=0;
    end;

    invalidate;
    c:=xpwindowmanager.framedclients[activetask];
    label1.caption:=getdottedText(c.gettitle,label1.width);
end;

end.
