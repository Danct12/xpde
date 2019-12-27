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
unit uGraphics;

interface

uses Classes, QGraphics, Types, QExtCtrls, QControls;


//Draw a 3D Rectangle y several styles
procedure Draw3DRect(const Canvas:TCanvas; Rect: TRect; const style: TBorderStyle);
//Draw a 3D Rectangle y several styles
procedure R3D(canvas:TCanvas;r:TRect;flat:boolean=false;raised:boolean=true;fill:boolean=true);

//Draw source1 over source2 and produces target
//Source1 can contain an alpha channel that will be merged with source2
procedure AlphaBitmap(source1,source2,target:TBitmap;dens:longint);

//Merge two bitmaps
procedure MergeBitmaps(source1,source2,target:TBitmap;dens:longint);

//Creates a selected bitmap
procedure SelectedBitmap(source1,source2,target:TBitmap;dens:longint);

//Copies binary save a bitmap, even with alpha channel
procedure bitblt(source:TBitmap;target:TBitmap;const x,y,w,h:integer);

//Creates a masked bitmap, should be deprecated
procedure MaskedBitmap(orig:TBitmap;result:TBitmap);

function rgbtocolor(r,g,b:byte):TColor;

procedure resizeBitmap(original, target: TBitmap; const width,height: integer; const border:integer=3);

implementation

function rgbtocolor(r,g,b:byte):TColor;
begin
   result:=b shl 16;
   result:=result+(g shl 8);
   result:=result+r;
end;

// Draw a 3D Rectangle in several styles
procedure Draw3DRect(const Canvas:TCanvas; Rect: TRect; const style: TBorderStyle);
const
  cColor: array [0..3] of Integer = (clBtnShadow, clBtnHighlight, clShadow, clMidLight);
begin
    { TODO : Add more drawing styles }
    Frame3D(Canvas, Rect, cColor[0], cColor[1], 1);
    Frame3D(Canvas, Rect, cColor[2], cColor[3], 1);
end;

procedure R3D(canvas:TCanvas;r:TRect;flat:boolean=false;raised:boolean=true;fill:boolean=true);
begin
    with canvas do begin
        if not flat then begin
            pen.mode:=pmCopy; 
            if raised then begin
                //Top Left
                pen.color:=clBtnFace;
                moveto(r.left,r.top);
                lineto(r.right-2,r.top);
                moveto(r.left,r.top);
                lineto(r.left,r.bottom-2);
                //Bottom Right
                pen.color:=clGray;
                moveto(r.right-1,r.top);
                lineto(r.right-1,r.bottom);
                moveto(r.left,r.bottom-1);
                lineto(r.right,r.bottom-1);
                //Top Left 2
                pen.color:=clWhite;
                moveto(r.left+1,r.top+1);
                lineto(r.right-2,r.top+1);
                moveto(r.left+1,r.top+1);
                lineto(r.Left+1,r.bottom-2);
                //Bottom Right 2
                pen.color:=clBtnShadow;
                moveto(r.right-2,r.top+1);
                lineto(r.right-2,r.bottom-1);
                moveto(r.left+1,r.bottom-2);
                lineto(r.right-1,r.bottom-2);
            end
            else begin
                //Top Left
                pen.color:=clBtnShadow;
                moveto(r.left,r.top);
                lineto(r.right-1,r.top);
                moveto(r.left,r.top);
                lineto(r.left,r.bottom-1);
                //Bottom Right
                pen.color:=clWhite;
                moveto(r.right-1,r.top+1);
                lineto(r.right-1,r.bottom);
                moveto(r.left+1,r.bottom-1);
                lineto(r.right,r.bottom-1);
                //Top Left 2
                pen.color:=clGray;
                moveto(r.left+1,r.top+1);
                lineto(r.right-2,r.top+1);
                moveto(r.left+1,r.top+1);
                lineto(r.Left+1,r.bottom-2);
                //Bottom Right 2
                pen.color:=clBtnFace;
                moveto(r.right-2,r.top+1);
                lineto(r.right-2,r.bottom-1);
                moveto(r.left+1,r.bottom-2);
                lineto(r.right-1,r.bottom-2);
            end;
            if fill then begin
                brush.Style:=bsSolid;
                inflaterect(r,-2,-2);
                fillrect(r);
            end;
        end
        else begin
            pen.mode:=pmCopy; 
            if raised then begin
                //Top Left
                pen.color:=clWhite;
                moveto(r.left,r.top);
                lineto(r.right-1,r.top);
                moveto(r.left,r.top);
                lineto(r.left,r.bottom-1);
                //Bottom Right
                pen.color:=clBtnShadow;
                moveto(r.right-1,r.top);
                lineto(r.right-1,r.bottom);
                moveto(r.left,r.bottom-1);
                lineto(r.right,r.bottom-1);
            end
            else begin
                //Top Left
                pen.color:=clBtnShadow;
                moveto(r.left,r.top);
                lineto(r.right-1,r.top);
                moveto(r.left,r.top);
                lineto(r.left,r.bottom-1);
                //Bottom Right
                pen.color:=clWhite;
                moveto(r.right-1,r.top);
                lineto(r.right-1,r.bottom);
                moveto(r.left,r.bottom-1);
                lineto(r.right,r.bottom-1);
            end;
            if fill then begin
                brush.Style:=bsSolid;
                inflaterect(r,-1,-1);
                fillrect(r);
            end;
        end;
    end;
end;

// Copies a bitmap
procedure bitblt(source:TBitmap;target:TBitmap;const x,y,w,h:integer);
var
   spoints: pointer;
   tpoints: pointer;
   t,l: integer;
begin
    for t:=y to (y+h)-1 do begin
        spoints:=source.ScanLine[t];
        tpoints:=target.scanline[t-y];
        inc(PChar(spoints),x*4);
        for l:=x to (x+w)-1 do begin
            integer(tpoints^):=integer(spoints^);
            inc(PChar(tpoints),4);
            inc(PChar(spoints),4);
        end;
    end;
end;

//Merge 2 bitmaps with transparencies
procedure MergeBitmaps(source1,source2,target:TBitmap;dens:longint);
var
    aEBX, aESI, aEDI, aESP, aEDX, Dens1, Dens2: Longint;
    i: longint;
    ptz: pointer;
    ptt: pointer;
    ptf: pointer;
    w:longint;
    bmz:TBitmap;
    bmf:TBitmap;
    bmt:TBitmap;
    fina:integer;
const
    Maxize = (1294967280 Div SizeOf(TPoint));
    MaxPixelCount = 32768;
    Mask0101 = $00FF00FF;
    Mask1010 = $FF00FF00;
begin
    bmz:=TBitmap.create;
    bmf:=TBitmap.create;
    bmt:=TBitmap.create;

    bmz.PixelFormat:=pf32bit;
    bmf.PixelFormat:=pf32bit;
    bmt.PixelFormat:=pf32bit;

    bmz.width:=source1.width;
    bmz.height:=source1.height;

    bmf.width:=source1.width;
    bmf.height:=source1.height;

    bmt.width:=source1.width;
    bmt.height:=source1.height;


    bmF.Canvas.brush.color:=clFuchsia;    
    bmF.Canvas.Draw(0, 0, source1);
    bmT.Canvas.Draw(0, 0, source2);
    bmZ.Canvas.Draw(0, 0, bmF);

    w:=bmz.width;
    for i := 0 to bmz.height - 1 do begin
        Ptz := bmz.Scanline[i];
        Ptt := bmt.Scanline[i];
        Ptf := bmf.Scanline[i];
            asm
		        MOV aEBX, EBX
		        MOV aEDI, EDI
		        MOV aESI, ESI
		        MOV aESP, ESP
		        MOV aEDX, EDX

		        MOV EBX, Dens
		        MOV Dens1, EBX

		        NEG BL
		        ADD BL, $20
		        MOV Dens2, EBX
		        CMP Dens1, 0
		        JZ  @Final

		        MOV EDI, ptz
		        MOV ESI, ptt
		        MOV ECX, ptf

		        MOV EAX, w
		        lea EAX, [EAX+EAX*2+3]
		        ADD EAX,w
		        AND EAX, $FFFFFFFC
		        ADD EAX, EDI
		        MOV FinA, EAX

		        MOV EDX,EDI
		        MOV ESP,ESI
		        MOV ECX,ECX

            @LOOPA:
		        MOV  EAX, [EDX]
		        MOV  EDI, [ESP]
		        MOV  EBX, EAX
		        AND  EAX, Mask1010
		        AND  EBX, Mask0101
		        SHR  EAX, 5
		        IMUL EAX, Dens2
		        IMUL EBX, Dens2
		        MOV  ESI, EDI
		        AND  EDI, Mask1010
		        AND  ESI, Mask0101
		        SHR  EDI, 5
		        IMUL EDI, Dens1
		        IMUL ESI, Dens1
		        ADD  EAX, EDI
		        ADD  EBX, ESI
		        AND  EAX, Mask1010
		        SHR  EBX, 5
		        AND  EBX, Mask0101
		        OR   EAX, EBX
		        MOV [ECX], EAX

		        ADD  EDX, 4
		        ADD  ESP, 4
		        ADD  ECX, 4

		        CMP  EDX, FinA
		        JNE  @LOOPA
            @final:
		        MOV EBX, aEBX
		        MOV EDI, aEDI
		        MOV ESI, aESI
		        MOV ESP, aESP
		        MOV EDX, aEDX
            end;
        end;
        target.assign(bmf);
        bmz.free;
        bmf.free;
        bmt.free;
end;


//Applies the alpha channel of a bitmap
procedure AlphaBitmap(source1,source2,target:TBitmap;dens:longint);
var
    aEBX, aESI, aEDI, aESP, aEDX, Dens1, Dens2: Longint;
    i: longint;
    ptz: pointer;
    ptt: pointer;
    ptf: pointer;
    w:longint;
    bmz:TBitmap;
    bmf:TBitmap;
    bmt:TBitmap;
    fina:integer;
const
    Maxize = (1294967280 Div SizeOf(TPoint));
    MaxPixelCount = 32768;
    Mask0101 = $00FF00FF;
    Mask1010 = $FF00FF00;
begin
    bmF:=source1;
    bmt:=source2;
    bmZ:=bmf;

    w:=bmz.width;

    for i := 0 to bmz.height - 1 do begin
        Ptz := bmz.Scanline[i];
        Ptt := bmt.Scanline[i];
        Ptf := bmf.Scanline[i];
            asm
		        MOV aEBX, EBX
		        MOV aEDI, EDI
		        MOV aESI, ESI
		        MOV aESP, ESP
		        MOV aEDX, EDX

		        MOV EBX, Dens
		        MOV Dens1, EBX

		        NEG BL
		        ADD BL, $20
		        MOV Dens2, EBX

		        MOV EDI, ptz
		        MOV ESI, ptt
		        MOV ECX, ptf

		        MOV EAX, w
		        lea EAX, [EAX+EAX*2+3]
		        ADD EAX,w
		        AND EAX, $FFFFFFFC
		        ADD EAX, EDI
		        MOV FinA, EAX

		        MOV EDX,EDI
		        MOV ESP,ESI
		        MOV ECX,ECX

            @LOOPA:
		        MOV  EAX, [EDX]
                SHR EAX, 24

		        MOV EBX, EAX
                ADD EBX, dens
                CMP EBX, $20
                JNG @p
                MOV EBX, $20
            @p:
		        MOV Dens1, EBX
		        NEG BL
		        ADD BL, $20
		        MOV Dens2, EBX
		        CMP Dens1, 0
		        JZ  @sig

		        MOV  EAX, [EDX]
		        MOV  EDI, [ESP]
                CMP EAX, $FFFFFFFF
                JZ @mask
             @again:
		        MOV  EBX, EAX
		        AND  EAX, Mask1010
		        AND  EBX, Mask0101
		        SHR  EAX, 5
		        IMUL EAX, Dens2
		        IMUL EBX, Dens2
		        MOV  ESI, EDI
		        AND  EDI, Mask1010
		        AND  ESI, Mask0101
		        SHR  EDI, 5
		        IMUL EDI, Dens1
		        IMUL ESI, Dens1
		        ADD  EAX, EDI
		        ADD  EBX, ESI
		        AND  EAX, Mask1010
		        SHR  EBX, 5
		        AND  EBX, Mask0101
		        OR   EAX, EBX
		        MOV [ECX], EAX
            @sig:
		        ADD  EDX, 4
		        ADD  ESP, 4
		        ADD  ECX, 4

		        CMP  EDX, FinA
		        JNE  @LOOPA
                JE  @final
            @mask:
		        MOV  EAX, EDI
		        MOV [ECX], EAX
                jmp  @sig
            @final:
		        MOV EBX, aEBX
		        MOV EDI, aEDI
		        MOV ESI, aESI
		        MOV ESP, aESP
		        MOV EDX, aEDX
            end;
        end;

        target.assign(bmf);
end;

//Creates the selected bitmap
procedure SelectedBitmap(source1,source2,target:TBitmap;dens:longint);
var
    aEBX, aESI, aEDI, aESP, aEDX, Dens1, Dens2, Dens3: Longint;
    i: longint;
    ptz: pointer;
    ptt: pointer;
    ptf: pointer;
    w:longint;
    bmz:TBitmap;
    bmf:TBitmap;
    bmt:TBitmap;
    fina:integer;
const
    Maxize = (1294967280 Div SizeOf(TPoint));
    MaxPixelCount = 32768;
    Mask0101 = $00FF00FF;
    Mask1010 = $FF00FF00;
begin
    bmz:=TBitmap.create;
    bmf:=TBitmap.create;
    bmt:=TBitmap.create;

    bmz.PixelFormat:=pf32bit;
    bmf.PixelFormat:=pf32bit;
    bmt.PixelFormat:=pf32bit;

    bmz.width:=source1.width;
    bmz.height:=source1.height;

    bmf.width:=source1.width;
    bmf.height:=source1.height;

    bmt.width:=source1.width;
    bmt.height:=source1.height;


    bmF.Assign(source1);
    bmt.assign(source2);
    bmZ.assign(bmf);

    w:=bmz.width;

    for i := 0 to bmz.height - 1 do begin
        Ptz := bmz.Scanline[i];
        Ptt := bmt.Scanline[i];
        Ptf := bmf.Scanline[i];
            asm
		        MOV aEBX, EBX
		        MOV aEDI, EDI
		        MOV aESI, ESI
		        MOV aESP, ESP
		        MOV aEDX, EDX

		        MOV EBX, Dens
		        MOV Dens1, EBX

		        NEG BL
		        ADD BL, $20
		        MOV Dens2, EBX
//		        CMP Dens1, 0
//		        JZ  @Final

		        MOV EDI, ptz
		        MOV ESI, ptt
		        MOV ECX, ptf

		        MOV EAX, w
		        lea EAX, [EAX+EAX*2+3]
		        ADD EAX,w
		        AND EAX, $FFFFFFFC
		        ADD EAX, EDI
		        MOV FinA, EAX

		        MOV EDX,EDI
		        MOV ESP,ESI
		        MOV ECX,ECX

            @LOOPA:
		        MOV  EAX, [EDX]
                SHR EAX, 24

		        MOV EBX, EAX
                ADD EBX, dens
                CMP EBX, $20
                JNG @p
                MOV EBX, $20
            @p:
		        MOV Dens1, EBX
		        NEG BL
		        ADD BL, $20
		        MOV Dens2, EBX
		        CMP Dens1, 0
		        JZ  @sig

		        MOV  EAX, [EDX]
                SHR EAX, 24
                SHL EAX, 24
                MOV Dens3, EAX

		        MOV  EAX, [EDX]
		        MOV  EDI, [ESP]
                CMP EAX, $FFFFFFFF
                JZ @mask
             @again:
		        MOV  EBX, EAX

		        AND  EAX, Mask1010
		        AND  EBX, Mask0101
		        SHR  EAX, 5
		        IMUL EAX, Dens2
		        IMUL EBX, Dens2
		        MOV  ESI, EDI
		        AND  EDI, Mask1010
		        AND  ESI, Mask0101
		        SHR  EDI, 5
		        IMUL EDI, Dens1
		        IMUL ESI, Dens1
		        ADD  EAX, EDI
		        ADD  EBX, ESI
		        AND  EAX, Mask1010
		        SHR  EBX, 5
		        AND  EBX, Mask0101
		        OR   EAX, EBX
                SHL EAX ,8
                SHR EAX ,8

                OR   EAX, Dens3
		        MOV [ECX], EAX
            @sig:
		        ADD  EDX, 4
		        ADD  ESP, 4
		        ADD  ECX, 4

		        CMP  EDX, FinA
		        JNE  @LOOPA
                JE  @final
            @mask:
//		        MOV  EAX, EDI
//		        MOV [ECX], EAX
                jmp  @sig
            @final:
		        MOV EBX, aEBX
		        MOV EDI, aEDI
		        MOV ESI, aESI
		        MOV ESP, aESP
		        MOV EDX, aEDX
            end;
        end;
        
        target.assign(bmf);
        bmz.free;
        bmf.free;
        bmt.free;
end;

procedure MaskedBitmap(orig:TBitmap;result:TBitmap);
var
    mask:TBitmap;
    source: TBitmap;
begin
    mask:=TBitmap.create;
    source:=TBitmap.create;
    try
        source.width:=orig.width;
        source.height:=orig.height;

        result.width:=orig.width;
        result.height:=orig.height;

        source.Canvas.brush.color:=clFuchsia;
        source.Canvas.draw(0,0,orig);
        source.transparent:=true;

        mask.width:=source.width;
        mask.height:=source.height;

        mask.Canvas.brush.color:=clHighLight;
        mask.Canvas.pen.color:=clHighLight;
        mask.Canvas.fillrect(rect(0,0,source.width,source.height));

        MergeBitmaps(source,mask,result,14);
    finally
        source.free;
        mask.free;
    end;
end;


procedure resizeBitmap(original, target: TBitmap; const width,height: integer; const border:integer=3);
var
    rRect: TRect;
    rTop: TRect;
    rLeft: TRect;
    rRight: TRect;
    rBottom: TRect;

    procedure stretchCopy(destCanvas: TCanvas; dest: TRect; sourceCanvas: TCanvas; source:TRect);
    var
        temp: TBitmap;
    begin
        temp:=TBitmap.create;
        try
            temp.width:=source.Right-source.Left;
            temp.height:=source.Bottom-source.Top;
            temp.Canvas.CopyRect(rect(0,0,temp.width,temp.height),sourcecanvas,source);
            destCanvas.StretchDraw(dest,temp);
        finally
            temp.free;
        end;
    end;
begin
    target.width:=width;
    target.height:=height;
    target.Canvas.Rectangle(0,0,width,height);

    rRect:=Rect(0,0,width,height);

    if (border<>0) then begin
        rTop:=rRect;
        rTop.Bottom:=border;

        rRight:=rRect;
        rRight.Left:=rRight.Right-border;

        rBottom:=rRect;
        rBottom.top:=rBottom.bottom-border;


        target.Canvas.CopyRect(rect(0,0,border,border),original.canvas,rect(0,0,border,border));
        target.Canvas.CopyRect(rect(width-border,0,width,border),original.canvas,rect(original.width-border,0,original.width,border));
        target.Canvas.CopyRect(rect(0,height-border,border,height),original.canvas,rect(0,original.height-border,border,original.height));
        target.Canvas.CopyRect(rect(width-border,height-border,width,height),original.canvas,rect(original.width-border,original.height-border,original.width,original.height));

        rLeft:=rRect;
        rLeft.top:=border;
        rLeft.right:=border;
        rLeft.bottom:=height-border;
        stretchCopy(target.canvas,rLeft,original.canvas,rect(0,border,border,original.height-border));

        rRight:=rRect;
        rRight.top:=border;
        rRight.left:=width-border;
        rRight.bottom:=height-border;
        stretchCopy(target.canvas,rRight,original.canvas,rect(original.width-border,border,original.width,original.height-border));

        rTop:=rRect;
        rTop.bottom:=border;
        rTop.left:=border;
        rTop.right:=width-border;
        stretchCopy(target.canvas,rTop,original.canvas,rect(border,0,original.width-border,border));

        rBottom:=rRect;
        rBottom.top:=height-border;
        rBottom.left:=border;
        rBottom.right:=width-border;
        stretchCopy(target.canvas,rBottom,original.canvas,rect(border,original.height-border,original.width-border,original.height));
    end;

    rRect.left:=border;
    rRect.top:=border;
    rRect.right:=width-border;
    rRect.bottom:=height-border;
    stretchCopy(target.canvas,rRect,original.canvas,rect(border,border,original.width-border,original.height-border));


end;



end.
