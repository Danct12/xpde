unit RegLib;

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
