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
unit uCommon;

interface

uses
    Libc, QThemed, QThemeSrvLinux,
    uRegistry, SysUtils, QForms,
    Classes, QDialogs, Inifiles;

const
    XP_HOME_DIR=0;
    XP_BASE_DIR=1;
    XP_DESKTOP_DIR=2;
    XP_THEMES_DIR=3;
    XP_CURRENT_THEME_DIR=4;
    XP_ICON_DIR=5;
    XP_NORMAL_SIZE_ICON_DIR=6;
    XP_WALLPAPERS_DIR=7;
    XP_MISC_DIR=8;
    XP_APP_DIR=9;
    XP_TASKBAR_DIR=10;
    XP_FRAME_DIR=11;
    XP_CURRENT_USER=12;
    XP_CURRENT_USER_REAL_NAME=13;
    XP_MEDIUM_SIZE_ICON_DIR=14;
    XP_START_MENU_DIR=15;
    XP_START_MENU_PROGRAMS_DIR=16;
    XP_SMALL_SIZE_ICON_DIR=17;
    XP_START_MENU_THEME_DIR=18;    

//Returns system info
function getSystemInfo(const item:integer):string;

//Returns the home dir
function getHomeDir: string;

//Stores the app dir in the registry
procedure storeAppDir;

//Returns the app dir
function getAppDir:string;

//Copies a file
function CopyFile(const Source, Destination: string): Boolean;

function ShellExecute(const executable:string): integer;




//Auxiliary functions
function min(const a,b: integer):integer;
function max(const a,b: integer):integer;
function GetTickCount: Cardinal;

//XLib
function listtostr(const str:string):string;

implementation


function getHomeDir: string;
begin
  result := getpwuid(getuid)^.pw_dir;
end;

function getCurrentUser: string;
begin
  result := getpwuid(getuid)^.pw_name;
end;

function getCurrentUserRealName: string;
begin
  result := getpwuid(getuid)^.pw_gecos;
  if (result='') then result:=getCurrentUser;
end;

function getPrivateDir: string;
begin
    result:=getHomeDir+'/.xpde';
end;

function getSystemInfo(const item:integer):string;
begin
    result:='';
    case item of
        XP_HOME_DIR               : result:=getHomeDir;
        XP_BASE_DIR               : result:=getPrivateDir;
        XP_DESKTOP_DIR            : result:=getPrivateDir+'/Desktop';
        XP_THEMES_DIR             : result:=getPrivateDir+'/Themes';
        XP_CURRENT_THEME_DIR      : result:=getSystemInfo(XP_THEMES_DIR)+'/'+themeservices.ThemeNames[themeservices.themeindex];
        XP_ICON_DIR               : result:=getSystemInfo(XP_CURRENT_THEME_DIR)+'/Icons';
        XP_NORMAL_SIZE_ICON_DIR   : result:=getSystemInfo(XP_ICON_DIR)+'/32x32';
        XP_MEDIUM_SIZE_ICON_DIR   : result:=getSystemInfo(XP_ICON_DIR)+'/22x22';
        XP_SMALL_SIZE_ICON_DIR    : result:=getSystemInfo(XP_ICON_DIR)+'/16x16';
        XP_WALLPAPERS_DIR         : result:=getSystemInfo(XP_CURRENT_THEME_DIR)+'/Wallpapers';
        XP_MISC_DIR               : result:=getSystemInfo(XP_CURRENT_THEME_DIR)+'/Misc';
        XP_TASKBAR_DIR            : result:=getSystemInfo(XP_CURRENT_THEME_DIR)+'/Taskbar';
        XP_START_MENU_THEME_DIR   : result:=getSystemInfo(XP_CURRENT_THEME_DIR)+'/StartMenu';
        XP_FRAME_DIR              : result:=getSystemInfo(XP_CURRENT_THEME_DIR)+'/Frame';
        XP_APP_DIR                : result:=getAppDir;
        XP_CURRENT_USER           : result:=getCurrentUser;
        XP_CURRENT_USER_REAL_NAME : result:=getCurrentUserRealName;
        XP_START_MENU_DIR         : result:=getPrivateDir+'/Start Menu';
        XP_START_MENU_PROGRAMS_DIR: result:=getSystemInfo(XP_START_MENU_DIR)+'/Programs';
    end;
end;

procedure storeAppDir;
var
    reg: TRegistry;
begin
    reg:=TRegistry.create;
    try
        if reg.OpenKey('Software/XPde/Desktop/Config', true) then begin
            reg.Writestring('BaseDir',extractfilepath(application.exename));
        end;
    finally
        reg.free;
    end;
end;

function getAppDir:string;
var
    reg: TRegistry;
begin
    result:='';
    reg:=TRegistry.create;
    try
        if reg.OpenKey('Software/XPde/Desktop/Config', false) then begin
            result:=reg.ReadString('BaseDir');
        end;
    finally
        reg.free;
    end;
end;

function CopyFile(const Source, Destination: string): Boolean;
var
    SourceStream: TFileStream;
begin
    Result := false;
    if not FileExists(Destination) then begin
        SourceStream := TFileStream.Create(Source, fmOpenRead);
        try
            with TFileStream.Create(Destination, fmCreate) do begin
                try
                    CopyFrom(SourceStream, 0);
                finally
                    Free;
                end;
            end;
        finally
            SourceStream.free;
        end;
        Result := true;
    end;
end;

function min(const a,b: integer):integer;
begin
    result:=a;
    if (b<a) then result:=b;
end;

function max(const a,b: integer):integer;
begin
    result:=a;
    if (b>a) then result:=b;
end;

function GetTickCount: Cardinal;
var
  tv: timeval;
begin
  gettimeofday(tv, nil);
  {$RANGECHECKS OFF}
  Result := int64(tv.tv_sec) * 1000 + tv.tv_usec div 1000;
end;

function listtostr(const str:string):string;
var
    k:integer;
    i: integer;
begin
    result:=str;
    k:=pos(#27,result);
    while (k<>0) do begin
        i:=pos(#2,result);
        Delete(result,k,i-k+1);
        k:=pos(#27,result);
    end;
end;

function ShellExecute(const executable:string): integer;
var
    prog:string;
    ini: TIniFile;
begin
    { TODO : Allow for system paths %value% }
    //prog:=replacesystempaths(theprog);
    //aprog:=prog;
    //if not waitfor then aprog:=aprog+' &';
    { TODO : Read for .lnk parameters to execute the appropiate command }
    if (ansilowercase(extractfileext(executable))='.lnk') then begin
        ini:=TIniFile.create(executable);
        try
            prog:=ini.ReadString('Shortcut','Command','');
            prog:=prog+' &';
        finally
            ini.free;
        end;
    end
    else prog:=executable+' &';

    result:=Libc.system(PChar(prog));
    if result = -1 then begin
        showmessage(format('Unable to execute %s',[prog]));
    end;
end;



end.
