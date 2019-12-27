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

//New version enhancements are marked with V2

unit uRegistry;

interface

uses
  Libc, Classes, Sysutils, StrUtils;
type
  ERegistryException = class(Exception);
  TRegDataType = (rdUnknown, rdString, rdExpandString, rdInteger, rdBinary);
  TRegDataInfo = record
    RegData: TRegDataType;
    DataSize: Integer;
  end;
//BEGIN V2
  TRegKeyInfo = record
    NumSubKeys: Integer;
    MaxSubKeyLen: Integer;
    NumValues: Integer;
    MaxValueLen: Integer;
  end;
//END V2
  TRegistry = class(TObject)
  private
    FRootKey: string;
    fCurrentPath: string;
    procedure SetRootKey(const Value: string);
    function GetRootPath: string;
    function GetCurrentPath: string;
    procedure PutData(const Name: string; Buffer: Pointer;
      BufSize: Integer; RegData: TRegDataType);
    function GetData(const Name: string; Buffer: Pointer;
      BufSize: Integer; var RegData: TRegDataType): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function OpenKey(const Key: string; CanCreate: Boolean): Boolean;
    procedure CloseKey;
    procedure DeleteKey;
    function DeleteValue(const Name: string): Boolean;
    function GetDataInfo(const ValueName: string; var Value: TRegDataInfo): Boolean;
    function Getdatatype(const Valuename: string): Tregdatatype;
    function Getdatasize(const Valuename: string): Integer;
    function KeyExists(const Key: string): Boolean;
    function Valueexists(const Name: string): Boolean;
    function Readinteger(const Name: string): Integer;
    function Readfloat(const Name: string): Double;
    function Readstring(const Name: string): string;
    function Readbool(const Name: string): Boolean;
    function Readdate(const Name: string): Tdatetime;
    function Readdatetime(const Name: string): Tdatetime;
    function Readtime(const Name: string): Tdatetime;
    function Readcurrency(const Name: string): Currency;
    function Readbinarydata(const Name: string; var Buffer; BufSize: Integer): Integer;
  //BEGIN V2
    function GetKeyInfo(var Value: TRegKeyInfo): Boolean;
    procedure GetKeyNames(Strings: TStrings);
    procedure GetValueNames(Strings: TStrings);
    procedure MoveKey(const OldName, NewName: string; Delete: Boolean);
    procedure RenameValue(const OldName, NewName: string);
    function ImportRegData(Filename: string): boolean;
    function ExportRegDataFile(Key, Filename: string): boolean;
    function ExportRegDataStrings(Key: string; StriLi:TStrings): boolean;
  //END V2
    procedure Writeinteger(const Name: string; Value: Integer);
    procedure Writefloat(const Name: string; Value: Double);
    procedure Writestring(const Name, Value: string);
    procedure Writebool(const Name: string; Value: Boolean);
    procedure Writedate(const Name: string; Value: Tdatetime);
    procedure Writedatetime(const Name: string; Value: Tdatetime);
    procedure Writetime(const Name: string; Value: Tdatetime);
    procedure Writecurrency(const Name: string; Value: Currency);
    procedure Writebinarydata(const Name: string; var Buffer; BufSize: Integer);
    property RootKey: string read FRootKey write SetRootKey;
    property RootPath: string read GetRootPath;
    property CurrentPath: string read GetCurrentPath;
  end;

function Regdatatodatatype(Value: Tregdatatype): Integer;
function Myfilesize(const Name: string): Longint;
procedure Deletefiles(Path: string);
function getHomeDir: string;

//BEGIN V2
procedure SplitKey(Inp: string; var Rootkey, Key: string);
function TranslateChar(const Str: string; FromChar, ToChar: Char): string;
procedure CountSubFolders(const Directory: string; var NumSubKeys, MaxSubKeyLen,
  NumValues, MaxValueLen: integer);
procedure CopyDirectoryTree(SourceDir, DestDir: string);
//END V2


const
  HKEY_CLASSES_ROOT = 'HKEY_CLASSES_ROOT';
  HKEY_CURRENT_USER = 'HKEY_CURRENT_USER';
  HKEY_LOCAL_MACHINE = 'HKEY_LOCAL_MACHINE';
  HKEY_USERS = 'HKEY_USERS';
  HKEY_PERFORMANCE_DATA = 'HKEY_PERFORMANCE_DATA';
  HKEY_CURRENT_CONFIG = 'HKEY_CURRENT_CONFIG';
  HKEY_DYN_DATA = 'HKEY_DYN_DATA';

  REG_SZ = 0;
  REG_EXPAND_SZ = 1;
  REG_DWORD = 2;
  REG_BINARY = 3;
  REG_NONE = 4;
  file_types: array[0..3] of string = ('.SZ', '.EXPAND', '.DWORD', '.BINARY');
implementation
uses
  RTLConsts, Math, IniFiles, QDialogs, uRegLib;

procedure Readerror(const Name: string);
begin
  raise ERegistryException.CreateResFmt(@SInvalidRegType, [Name]);
end;
{ TRegistry }

constructor TRegistry.Create;
begin
  FRootKey := GetRootPath + '/' + HKEY_CURRENT_USER;
  fCurrentPath := '';
end;

destructor TRegistry.Destroy;
begin
  inherited;
end;

function Regdatatodatatype(Value: Tregdatatype): Integer;
begin
  case Value of
    rdString: Result := REG_SZ;
    rdExpandString: Result := REG_EXPAND_SZ;
    rdInteger: Result := REG_DWORD;
    rdBinary: Result := REG_BINARY;
  else
    Result := REG_NONE;
  end;
end;

function Tregistry.GetCurrentPath: string;
begin
  result := fCurrentPath + '/';
end;

function Tregistry.GetRootPath: string;
begin
  result := getHomeDir + '/.registry';
end;

function Tregistry.Openkey(const Key: string; Cancreate: Boolean): Boolean;
begin
  fCurrentPath := FRootKey + '/' + Key;
  if cancreate then
  begin
    result := ForceDirectories(fCurrentPath);
  end
  else
  begin
    result := DirectoryExists(fCurrentPath);
  end;
end;

procedure Tregistry.Closekey;
begin
  fCurrentPath := '';
end;

function Tregistry.Readinteger(const Name: string): Integer;
var
  RegData: TRegDataType;
begin
  GetData(Name, @Result, SizeOf(Integer), RegData);
  if RegData <> rdInteger then ReadError(Name);
end;

procedure Tregistry.Writeinteger(const Name: string; Value: Integer);
begin
  PutData(Name, @Value, SizeOf(Integer), rdinteger);
end;

function Tregistry.Readstring(const Name: string): string;
var
  Len: Integer;
  RegData: TRegDataType;
begin
  Len := GetDataSize(Name);
  if Len > 0 then
  begin
    SetString(Result, nil, Len);
    GetData(Name, PChar(Result), Len, RegData);
    if (RegData = rdString) or (RegData = rdExpandString) then
      SetLength(Result, StrLen(PChar(Result)))
    else ReadError(Name);
  end
  else Result := '';
end;

procedure Tregistry.Writestring(const Name, Value: string);
begin
  PutData(Name, Pchar(Value), Length(Value) + 1, rdstring);
end;

function Tregistry.Readbool(const Name: string): Boolean;
begin
  Result := Boolean(ReadInteger(Name));
end;

procedure Tregistry.Writebool(const Name: string; Value: Boolean);
begin
  WriteInteger(Name, Integer(Value));
end;

procedure Tregistry.Writefloat(const Name: string; Value: Double);
begin
  PutData(Name, @Value, SizeOf(Double), rdBinary);
end;

function Tregistry.Readfloat(const Name: string): Double;
var
  Len: Integer;
  RegData: TRegDataType;
begin
  Len := GetData(Name, @Result, SizeOf(Double), RegData);
  if (RegData <> rdBinary) or (Len <> SizeOf(Double)) then
    ReadError(Name);
end;

procedure Tregistry.Writedatetime(const Name: string; Value: Tdatetime);
begin
  PutData(Name, @Value, SizeOf(TDateTime), rdBinary);
end;

function Tregistry.Readdatetime(const Name: string): Tdatetime;
var
  Len: Integer;
  RegData: TRegDataType;
begin
  Len := GetData(Name, @Result, SizeOf(TDateTime), RegData);
  if (RegData <> rdBinary) or (Len <> SizeOf(TDateTime)) then
    ReadError(Name);
end;

procedure Tregistry.Writedate(const Name: string; Value: Tdatetime);
begin
  WriteDateTime(Name, Value);
end;

function Tregistry.Readdate(const Name: string): Tdatetime;
begin
  Result := ReadDateTime(Name);
end;

procedure Tregistry.Writetime(const Name: string; Value: Tdatetime);
begin
  WriteDateTime(Name, Value);
end;

function Tregistry.Readtime(const Name: string): Tdatetime;
begin
  Result := ReadDateTime(Name);
end;

procedure Tregistry.Writecurrency(const Name: string; Value: Currency);
begin
  PutData(Name, @Value, SizeOf(Currency), rdBinary);
end;

function Tregistry.Readcurrency(const Name: string): Currency;
var
  Len: Integer;
  RegData: TRegDataType;
begin
  Len := GetData(Name, @Result, SizeOf(Currency), RegData);
  if (RegData <> rdBinary) or (Len <> SizeOf(Currency)) then
    ReadError(Name);
end;

procedure Tregistry.Writebinarydata(const Name: string; var Buffer;
  BufSize: Integer);
begin
  PutData(Name, @Buffer, BufSize, rdBinary);
end;

function Tregistry.Readbinarydata(const Name: string; var Buffer;
  BufSize: Integer): Integer;
var
  RegData: TRegDataType;
  Info: TRegDataInfo;
begin
  if GetDataInfo(Name, Info) then
  begin
    Result := Info.DataSize;
    RegData := Info.RegData;
    if ((RegData = rdBinary)) and (Result <= BufSize) then
      GetData(Name, @Buffer, Result, RegData)
    else ReadError(Name);
  end
  else
    Result := 0;
end;

function Tregistry.Getdata(const Name: string; Buffer: Pointer;
  BufSize: Integer; var RegData: TRegDataType): Integer;
var
  filename: string;
  m: TMemoryStream;
  di: TRegDataInfo;
begin
  result := -1;
  if (fCurrentPath = '') or (fRootKey = '') or (fCurrentPath = fRootKey) then
    raise ERegistryException.CreateResFmt(@SRegGetDataFailed, [Name]) else
  begin
    if GetDataInfo(Name, di) then
    begin
      filename := fCurrentPath + '/' + Name + file_types[RegDataToDataType(di.RegData)];
      if fileexists(filename) then
      begin
        m := TMemoryStream.create;
        try
          m.LoadFromFile(filename);
          m.position := 0;
          m.Read(Buffer^, BufSize);
          Result := di.DataSize;
          RegData := di.RegData;
        finally
          m.free;
        end;
      end;
    end;
  end;
end;

procedure Tregistry.Putdata(const Name: string; Buffer: Pointer;
  BufSize: Integer; RegData: TRegDataType);
var
  DataType: Integer;
  filename: string;
  m: TMemoryStream;
begin
  if (fCurrentPath = '') or (fRootKey = '') or (fCurrentPath = fRootKey) then
    raise ERegistryException.CreateResFmt(@SRegSetDataFailed, [Name]) else
  begin
    DataType := RegDataToDataType(RegData);
    filename := fCurrentPath + '/' + Name + file_types[DataType];
    m := TMemoryStream.create;
    try
      m.write(buffer^, BufSize);
      m.position := 0;
      m.SaveToFile(filename);
    finally
      m.free;
    end;
  end;
end;

procedure Tregistry.Setrootkey(const Value: string);
begin
  FRootKey := GetRootPath + '/' + Value;
end;

function TRegistry.KeyExists(const Key: string): Boolean;
begin
  Result := DirectoryExists(fRootKey + PathDelim + Key);
end;

function Tregistry.Valueexists(const Name: string): Boolean;
var
  sr: TRegDataInfo;
begin
  result := GetDataInfo(Name, sr)
end;

function Tregistry.Getdatainfo(const Valuename: string;
  var Value: TRegDataInfo): Boolean;
var
  KeyName: string;
begin
  KeyName := fCurrentPath + '/' + ValueName;
  Value.RegData := rdunknown;
  Result := false;
  if fileexists(KeyName + file_types[REG_SZ]) then
  begin
    Value.RegData := rdString;
    Value.DataSize := MyFileSize(KeyName + file_types[REG_SZ]);
  end
  else
    if fileexists(KeyName + file_types[REG_DWORD]) then
    begin
      Value.RegData := rdInteger;
      Value.DataSize := MyFileSize(KeyName + file_types[REG_DWORD]);
    end
    else
      if fileexists(KeyName + file_types[REG_BINARY]) then
      begin
        Value.RegData := rdBinary;
        Value.DataSize := MyFileSize(KeyName + file_types[REG_BINARY]);
      end;
  if Value.RegData <> rdunknown then result := true;
end;

function Tregistry.Getdatasize(const Valuename: string): Integer;
var
  Info: TRegDataInfo;
begin
  if GetDataInfo(ValueName, Info) then
    Result := Info.DataSize else
    Result := -1;
end;

function Tregistry.Getdatatype(const Valuename: string): Tregdatatype;
var
  Info: TRegDataInfo;
begin
  if GetDataInfo(ValueName, Info) then
    Result := Info.RegData else
    Result := rdUnknown;
end;

procedure Tregistry.Deletekey;
begin
  if (fCurrentPath = '') or (fRootKey = '') or (fCurrentPath = fRootKey) then
    raise ERegistryException.CreateFmt('Cannot delete Key ', [fCurrentPath]) else
  begin
    DeleteFiles(fCurrentPath);
    RemoveDir(fCurrentPath);
  end;
end;

//NEW

function TRegistry.DeleteValue(const Name: string): Boolean;
begin
  result:=false;
  if FileExists(fCurrentPath + PathDelim + Name + file_types[REG_DWORD]) then
    result:=DeleteFile(fCurrentPath + PathDelim + Name + file_types[REG_DWORD]) else
    if FileExists(fCurrentPath + PathDelim + Name + file_types[REG_SZ]) then
      result:=DeleteFile(fCurrentPath + PathDelim + Name + file_types[REG_SZ]) else
      if FileExists(fCurrentPath + PathDelim + Name + file_types[REG_BINARY]) then
        result:=DeleteFile(fCurrentPath + PathDelim + Name + file_types[REG_BINARY]);
end;

function CleanPath(Path:String):String;
const delim='\';
begin
  if Path<>'' then
  begin
  Result:=StringReplace(Path,Delim+Delim,Delim,[rfReplaceAll]);
  if Result<>'' then if Result[1]=Delim then Result:=Copy(Result,2,Length(Result));
  if Result<>'' then if Result[Length(Result)]=Delim then Result:=Copy(Result,1,Length(Result)-1);
  end else Result:='';
end;

function TRegistry.ExportRegDataFile(Key, Filename: string): boolean;
Var ExpData:TStrings;
begin
 result:=true; 
 ExpData:=TStringList.create;
 ExpData.Add('REGEDIT4');
 ExpData.Add('');
 if ExportRegDataStrings(Key,ExpData) then
 ExpData.SaveToFile(FileName);
 ExpData.free;
end;

function TRegistry.ExportRegDataStrings(Key: string; StriLi:TStrings): boolean;

var RtKey: string;
  DTAI: TRegDataInfo;
  fData: TMemoryStream;


  procedure AddValues(KeyName: string);
  var Str: TStrings;
    i: integer;
    ValStr: string;
    buf: byte;
  begin
    OpenKey(KeyName, false);  
    Str := TStringList.Create;
    GetValueNames(Str);
    for i := 0 to Str.Count - 1 do
    begin
      GetDataInfo(Str[i], DTAI);

      case DTAI.RegData of
        rdString: ValStr := '"' + ReadString(Str[i]) + '"';
        rdInteger: ValStr := 'dword:' + lowercase(InttoHex(ReadInteger(Str[i]), 8));
        rdBinary: begin
            fData.SetSize(DTAI.DataSize);
            Readbinarydata(Str[i], fData.Memory^, fData.Size);
            ValStr := 'hex:';
            fData.Position := 0;

            while fData.Position < fData.Size do
            begin
              fData.ReadBuffer(buf, 1);
              ValStr := ValStr + InttoHex(buf, 2);
              if fData.Position < fData.Size then ValStr := ValStr + ',';
            end;
          end;
      else ValStr := ''
      end;
      StriLi.Add('"' + Str[i] + '"=' + ValStr);
    end;
    Str.free;
  end;


  procedure CollectKey(KeyName: string);
  var
    i: integer;
    Str: TStrings;
  begin
    Str := TStringList.Create;
    OpenKey(KeyName, false);
    GetKeyNames(Str);
    for i := 0 to Str.Count - 1 do
    begin
      StriLi.Add('');

      StriLi.Add('[' + CleanPath(TranslateChar(RtKey + PathDelim + KeyName + PathDelim + Str[i], '/', '\')) + ']');

      AddValues(CleanPath(KeyName + PathDelim + Str[i]));
      CollectKey(CleanPath(KeyName + PathDelim + Str[i]));
    end;
    Str.free;
  end;

begin
  result:=True;
  fData := TMemoryStream.create;
  RtKey := Trim(Copy(RootKey, Length(GetRootPath) + 2, Length(RootKey)));

  if KeyExists(Key) then
  begin
  if (RtKey<>'') and (Key<>'') then
  begin
  StriLi.Add('[' + CleanPath(TranslateChar(RtKey + PathDelim + Key, '/', '\')) + ']');
  AddValues(CleanPath(key));
  end;
  CollectKey(Key);
  end else
    raise ERegistryException.CreateResFmt(@SRegCreateFailed, [Key]);
  fData.free;
end;

function TRegistry.ImportRegData(Filename: string): boolean;
var ini: TMemIniFile;
  KeysList, ValueNamesList, ValuesList: TStrings;
  i, ui, count, vl: integer;
  RK, Key, VNS, VAL, TMP: string;
  MemStr: TMemoryStream;
  byto: Byte;
  WC: PWideChar;
  PTMP: PChar;
begin
  KeysList := TStringList.Create;
  ValueNamesList := TStringList.Create;
  ValuesList := TStringList.Create;
  MemStr := TMemoryStream.create;
  MemStr.LoadFromFile(FileName);
  MemStr.Position := 0;
  MemStr.ReadBuffer(byto, 1);
  MemStr.Position := 0;
  ini := TMemIniFile.Create('');
  if byto = 82 then //The 'R' of 'REGEDIT4' ANSI
  begin
    PTMP := MemStr.Memory;
    KeysList.text := PTMP;
    ini.SetStrings(KeysList);
  end else
  begin //'?Windows Registry Editor Version 5.00' UNICODE
    WC := MemStr.Memory;
    KeysList.text := OleStrToString(WC);
    ini.SetStrings(KeysList);
  end;

  MemStr.free;
  ini.ReadSections(KeysList);

  for i := 0 to KeysList.count - 1 do
  begin
    SplitKey(KeysList[i], RK, Key);
    RootKey := RK;
    OpenKey(Key, true);
    ini.ReadSection(KeysList[i], ValueNamesList);
    for ui := 0 to ValueNamesList.count - 1 do
    begin

      VNS := AnsiDeQuotedStr(ValueNamesList[ui], '"');
      VAL := ini.ReadString(KeysList[i], ValueNamesList[ui], '');

      if (VAL<>'') and (VAL[1] = '"') then
      begin
        VAL := AnsiDeQuotedStr(VAL, '"');
        if VAL='""' then VAL:='';
        WriteString(VNS, VAL);
      end else
        if AnsiStartsStr('dword:', VAL) then
        begin
          VAL := Copy(VAL, 7, Length(VAL));
          WriteInteger(VNS, HexStringToInt(VAL));
        end else
          if AnsiStartsStr('hex:', VAL) then
          begin
            VAL := Copy(VAL, 5, Length(VAL));
            MemStr := TMemoryStream.create;

            count := 0;
            vl := Length(VAL);
            while count < vl do
            begin
              repeat
                TMP := TMP + VAL[count];
                inc(count);
              until (Val[count] = ',') or (count > vl);
              inc(count);
              byto := HexStringToInt(TMP);

              MemStr.WriteBuffer(byto, 1);
              TMP := '';
            end;

            MemStr.Position := 0;

            WriteBinaryData(VNS, MemStr.Memory^, MemStr.Size);
            MemStr.free;
          end;
    end;
  end;
  ini.free;
  KeysList.free;
  ValueNamesList.free;
  ValuesList.free;
  Result := true;
end;

procedure TRegistry.RenameValue(const OldName, NewName: string);
var
  Len: Integer;
  RegData: TRegDataType;
  Buffer: PChar;
begin
  if ValueExists(OldName) and not ValueExists(NewName) then
  begin
    Len := GetDataSize(OldName);
    if Len > 0 then
    begin
      Buffer := AllocMem(Len);
      try
        Len := GetData(OldName, Buffer, Len, RegData);
        DeleteValue(OldName);
        PutData(NewName, Buffer, Len, RegData);
      finally
        FreeMem(Buffer);
      end;
    end;
  end;
end;

function TRegistry.GetKeyInfo(var Value: TRegKeyInfo): Boolean;
begin
  result:=true;
  FillChar(Value, SizeOf(TRegKeyInfo), 0);
  CountSubFolders(fCurrentPath, Value.NumSubKeys, Value.MaxSubKeyLen,
    Value.NumValues, Value.MaxValueLen);
end;

procedure TRegistry.GetKeyNames(Strings: TStrings);
var
  SearchRec: TSearchRec;
  Attributes: Integer;
begin
  Strings.Clear;
  Attributes := faAnyFile;
  if FindFirst(IncludeTrailingPathDelimiter(fCurrentPath) + '*',
    Attributes, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) > 0 then
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          if Trim(SearchRec.Name) <> '' then Strings.Add(SearchRec.Name);
        end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

procedure TRegistry.GetValueNames(Strings: TStrings);
var
  SearchRec: TSearchRec;
  Attributes: Integer;
begin
  Strings.Clear;
  Attributes := faAnyFile;
  if FindFirst(IncludeTrailingPathDelimiter(fCurrentPath) + '*',
    Attributes, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) > 0 then
      begin
        //
      end else
      begin
        if Trim(SearchRec.Name) <> '' then Strings.Add(Copy(SearchRec.Name, 1, Length(SearchRec.Name) - Length(ExtractFileExt(SearchRec.Name))));
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

procedure TRegistry.MoveKey(const OldName, NewName: string; Delete: Boolean);
begin
  if Delete then
    RenameFile(fRootKey + PathDelim + OldName, fRootKey + PathDelim + NewName) else
    CopyDirectoryTree(fRootKey + PathDelim + OldName, fRootKey + PathDelim + NewName);
end;

//END NEW



//***************** Helpers *********************************

function TranslateChar(const Str: string; FromChar, ToChar: Char): string;
var
  I: Integer;
begin
  Result := Str;
  for I := 1 to Length(Result) do
    if Result[I] = FromChar then
      Result[I] := ToChar;
end;

procedure SplitKey(Inp: string; var Rootkey, Key: string);
var poso: integer;
begin
  Inp := TranslateChar(Inp, '\', '/');
  poso := pos('/', Inp);
  RootKey := Copy(Inp, 1, Poso - 1);
  Key := Copy(Inp, Poso + 1, Length(Inp));
end;

procedure Deletefiles(Path: string);
var
  SearchRec: TSearchRec;
  Found: Integer;
begin
  if IsPathDelimiter(Path, length(Path)) then Path := copy(Path, 1, length(Path) - 1);
  Found := FindFirst(Path + PathDelim + '*', faAnyFile, SearchRec);
  if Found = 0 then
  begin
    while Found = 0 do
    begin
      if (SearchRec.Attr and faDirectory) = faDirectory then
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          DeleteFiles(Path + PathDelim + SearchRec.Name);
          RemoveDir(Path + PathDelim + SearchRec.Name);
        end;
      end
      else
      begin
        DeleteFile(Path + PathDelim + SearchRec.Name);
      end;
      Found := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
  end;
end;

function Myfilesize(const Name: string): Longint;
var
  SRec: TSearchRec;
begin
  if FindFirst(name, faAnyfile, SRec) = 0 then
  begin
    Result := SRec.Size;
    Sysutils.FindClose(SRec);
  end
  else
    Result := 0;
end;

function getHomeDir: string;
begin
  result := getpwuid(getuid)^.pw_dir;
end;
//New

procedure CountSubFolders(const Directory: string; var NumSubKeys, MaxSubKeyLen,
  NumValues, MaxValueLen: integer);
var
  SearchRec: TSearchRec;
  Attributes: Integer;
begin
  NumSubKeys := 0;
  MaxSubKeyLen := 0;
  NumValues := 0;
  MaxValueLen := 0;
  Attributes := faAnyFile;
  if FindFirst(IncludeTrailingPathDelimiter(Directory) + '*',
    Attributes, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) > 0 then
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          inc(NumSubKeys);
          MaxSubKeyLen := Max(MaxSubKeyLen, Length(SearchRec.Name));
        end;
      end else
      begin
        inc(NumValues);
        MaxValueLen := Max(MaxValueLen, Length(SearchRec.Name));
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

procedure CopyDirectoryTree(SourceDir, DestDir: string);
var Command: string;
begin
  if DirectoryExists(DestDir) then
  begin
    raise Exception.Create('CopyDirectoryTree failed: Target Exists');
    Exit;
  end;
  Command := 'cp -r -x ' + SourceDir + ' ' + DestDir;
  if LibC.System(Pchar(Command)) <> 0 then
    raise Exception.Create('CopyDirectoryTree failed');
end;

//New
end.


