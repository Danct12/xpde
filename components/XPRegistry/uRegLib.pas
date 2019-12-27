{ *************************************************************************** }
{                                                                             }
{ This file is part of the XPde project                                       }
{                                                                             }
{ Copyright (c) 2002 Theo Lustenberger                                        }
{                                                                             }
{ This software is provided "as-is".  This software comes without warranty    }
{ or garantee, explicit or implied.  Use this software at your own risk.      }
{ The author will not be liable for any damage to equipment, data, or         }
{ information that may result while using this software.                      }
{                                                                             }
{ By using this software, you agree to the conditions stated above.           }
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
unit uRegLib;

interface

function HexStringToInt(HexString: string): cardinal;

implementation

procedure _HexVal(s: string; var V: LongWord; var Code: LongInt);
begin
  asm
push eax
push ebx
push edx
xor ebx,ebx
{$IFOPT H+}
xor eax, eax
mov edi,dword ptr [S]
{$ELSE}
mov eax,1
{$ENDIF}
@Loop:
{$IFOPT H+}
mov dl,byte ptr [edi+eax]
cmp dl,0
je @Ready
{$ELSE}
cmp al,byte ptr [S]
je @Ready
mov dl,byte ptr [S+eax]
{$ENDIF}
cmp dl,'0'
jb @Error
cmp dl,'9'
jbe @Num
and dl,0DFh
cmp dl,'A'
jb @Error
cmp dl,'F'
ja @Error
//Zeichen A..F
clc
sub dl,37h
jmp @Continue
//Zeichen 0..9
@Num:
and dl,0Fh
//Nibble
@Continue:
shl ebx,4
or bl,dl
inc eax
jmp @Loop
@Error:
{$IFOPT H+}
inc eax
{$ENDIF}
mov edi,[Code]
mov dword ptr [edi],eax
@Ready:
pop edx
mov edi,[V]
mov dword ptr [edi],ebx
pop ebx
pop eax
  end;

end;

{ ************************************************************************* }

function HexStringToInt(HexString: string): Cardinal;
var v: Longword;
  code: LongInt;
begin
  code := 0;
  v := 0;
  _HexVal(HexString, v, code);
  if code = 0 then RESULT := v else
    RESULT := -code;
end;

{ ************************************************************************* }


end.
