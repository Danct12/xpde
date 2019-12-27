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
unit uXPDirectoryMonitor;

{ TODO : Clean up the code }
interface

uses
    Classes, SysUtils, QFileCtrls,
    QTypes, QDialogs, Libc,
    QForms, QStdCtrls, QExtCtrls;

type
    //Dirty class to access the private FFiles field
    TDirtyDirectory = class(TPersistent)
    private
        FAutoUpdate: Boolean;
        FIncludeParentDir: Boolean;
        FSortMode: TSortMode;
        FFileType: TFileType;
        FSuspendEvents: Boolean;
        FDirChanging: Boolean;
        FClient: IDirectoryClient;
        FFiles: TList;
    end;

    //XPDirectory, maintains an updated list of the contents
    //of a directory
    TXPDirectory=class(TDirectory)
    private
        FOldLocation: string;
        FTimer: TTimer;
        doTimer: boolean;
        signal: boolean;
        function updatefilelist(const newlist:TStringList): boolean;
    public
        procedure OnTimer(sender: TObject);
        procedure ListFiles(Reread: Boolean = True); override;
        constructor Create(AClient: IDirectoryClient); override;
        destructor Destroy;override;
    end;

    //Wrapper around a TXPDirectory, this is the one you have to use
    //access directory properties through Directory property
    TXPDirectoryMonitor=class(TComponent, IDirectoryClient)
    private
        FDirectory: TXPDirectory;
        FOnDirectoryModified: TNotifyEvent;
        procedure SetOnDirectoryModified(const Value: TNotifyEvent);
    public
        function FileFound(const SearchRec: TSearchRec): Boolean;
        procedure ListEnd;
        function ListStart: Boolean;
        procedure DirectoryChanged(const NewDir: WideString);
        procedure MaskChange(const NewMask: WideString);

        constructor Create(AOwner:TComponent);override;
        destructor Destroy;override;
    published
        property Directory: TXPDirectory read FDirectory;
        property OnDirectoryModified: TNotifyEvent read FOnDirectoryModified write SetOnDirectoryModified;
    end;

implementation

{ TXPDirectoryMonitor }

constructor TXPDirectoryMonitor.Create(AOwner: TComponent);
begin
  inherited;
  FOnDirectoryModified:=nil;
  //Creates the directory object
  FDirectory:=TXPDirectory.create(self);
  FDirectory.AutoUpdate:=true;
  FDirectory.IncludeParentDir:=false;
end;

destructor TXPDirectoryMonitor.Destroy;
begin
  FDirectory.free;
  inherited;
end;

procedure TXPDirectoryMonitor.DirectoryChanged(const NewDir: WideString);
begin
    //Not used
end;

function TXPDirectoryMonitor.FileFound(const SearchRec: TSearchRec): Boolean;
begin
    //Not used
    result:=true;
end;

procedure TXPDirectoryMonitor.ListEnd;
begin
    //Fires the DirectoryModified event to update the control
    if assigned(FOnDirectoryModified) then FOnDirectoryModified(self);
end;

function TXPDirectoryMonitor.ListStart: Boolean;
begin
    //Not used
    result:=true;
end;

procedure TXPDirectoryMonitor.MaskChange(const NewMask: WideString);
begin
    //Not used
end;

procedure TXPDirectoryMonitor.SetOnDirectoryModified(const Value: TNotifyEvent);
begin
  FOnDirectoryModified := Value;
end;

{ TXPDirectory }

constructor TXPDirectory.Create(AClient: IDirectoryClient);
begin
  inherited;
  signal:=false;
  dotimer:=false;
  //Uses a timer to reduce update operations
  { TODO : Allow configure this by the registry } 
  FTimer:=TTimer.create(nil);
  FTimer.Interval:=500;
  FTimer.OnTimer:=OnTimer;
  FTimer.Enabled:=false;
  FOldLocation:='';
end;

destructor TXPDirectory.Destroy;
begin
    FTimer.free;
    inherited;
end;

procedure TXPDirectory.ListFiles(Reread: Boolean);
var
  SearchRec: TSearchRec;
  files: TStringList;
begin
    //If there are no files or location changed, reread all the contents
    if (Count=0) or (FOldLocation<>Location) then begin
        FOldLocation:=Location;
        inherited ListFiles(Reread);
    end
    else begin
        if ReRead and dotimer then begin
            //Start filling newlist
            files:=TStringList.create;
            try
                if FindFirst(IncludeTrailingPathDelimiter(Location) + '*', faAnyFile, SearchRec) = 0 then begin
                    repeat
                        if ((SearchRec.Name <> '..') and (SearchRec.Name <> '.')) then begin
                            files.addobject(searchrec.Name,TObject(AllocFileInfo(SearchRec)));
                        end;
                        application.processmessages;
                    until FindNext(SearchRec) <> 0;
                    FindClose(SearchRec);
                end;
                files.Sorted:=true;
                //Update the list with the new contents
                if updatefilelist(files) then begin
                    DoListEnd;
                end;
            finally
                files.free;
            end;
        end
        else begin
            if not ftimer.enabled then begin
                signal:=false;
                ftimer.enabled:=true;
            end
            else begin
                signal:=true;
            end;
        end;
    end;

end;

procedure TXPDirectory.OnTimer(sender: TObject);
begin
    FTimer.Enabled:=false;
    if signal then begin
        signal:=false;
        ftimer.Enabled:=true;
    end
    else begin
        dotimer:=true;
        try
            ListFiles;
        finally
            dotimer:=false;
        end;
    end;
end;

function TXPDirectory.updatefilelist(const newlist: TStringList): boolean;
var
    foldlist: TList;
    old, new: PFileInfo;
    fname: string;
    k: integer;
    i: integer;
begin
    result:=false;
    foldlist:=TDirtyDirectory(self).FFiles;

    //Iterates through the old list
    for i:=foldlist.count-1 downto 0 do begin
        old:=PFileInfo(foldlist[i]);
        fname:=old^.SR.Name;

        //Delete items that already exist
        if newlist.find(fname,k) then newlist.Delete(k)
        else begin
            //If the items doesn't exist on the new list, then is because have been removed from disk
            Dispose(PFileInfo(FoldList[i]));
            foldlist.Delete(i);
            result:=true;
        end;
        application.processmessages;
    end;

    //Add new items at the end
    for k:=0 to newlist.count-1 do begin
        new:=PFileInfo(newlist.Objects[k]);
        FOldList.Add(AllocFileInfo(new^.sr));
        result:=true;
        application.processmessages;
    end;
end;

end.
