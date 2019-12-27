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
unit uXPShellListView;

interface

uses
    Classes, QDialogs, Types,
    uXPListView, uXPDirectoryMonitor, uXPImageList,
    SysUtils, QTypes, uRegistry,
    uCommon, Inifiles, Libc;

type
    TXPShellListView=class(TXPListView)
    private
        FDirectory: string;
        FDirectoryMonitor: TXPDirectoryMonitor;
        FPositions: TStringList;
        FSystemIcons: integer;
        FOnGetSystemImage: TXPListViewGetImageIndexEvent;
        FOnGetSystemCaption: TXPListViewGetItemCaptionEvent;
        FIconLocationsKey: string;
        procedure saveLocation(const iconName:string; pos: TPoint);
        procedure deleteLocation(const iconName:string);
        function loadLocation(const iconName:string; var pos: TPoint):boolean;
        procedure SetDirectory(const Value: string);
        procedure clearPositions;
        { TODO : Create the class TXPCustomListView to set public properties and do... for event handlers }
        function doongetimage(Sender:TObject; const itemindex:integer):integer;
        function doongetitemcaption(Sender:TObject; const itemindex:integer):string;
        procedure doongetitempos (Sender:TObject; const itemindex:integer; var pos: TPoint);
        procedure doonsetitempos (Sender:TObject; const itemindex:integer; pos: TPoint);
        procedure SetSystemIcons(const Value: integer);
        procedure SetOnGetSystemCaption(const Value: TXPListViewGetItemCaptionEvent);
        procedure SetOnGetSystemImage(const Value: TXPListViewGetImageIndexEvent);
        procedure SetIconLocationsKey(const Value: string);
    public
        procedure dblclick;override;
        procedure updatePath(sender:TObject);
        constructor Create(AOwner:TComponent);override;
        destructor Destroy;override;
    published
        property Directory: string read FDirectory write SetDirectory;
        property SystemIcons: integer read FSystemIcons write SetSystemIcons;
        property IconLocationsKey: string read FIconLocationsKey write SetIconLocationsKey;
        property OnGetSystemImage: TXPListViewGetImageIndexEvent read FOnGetSystemImage write SetOnGetSystemImage;
        property OnGetSystemCaption: TXPListViewGetItemCaptionEvent read FOnGetSystemCaption write SetOnGetSystemCaption;
    end;

implementation


{ TXPShellListView }

procedure TXPShellListView.clearPositions;
var
    i:longint;
    p:PPoint;
begin
    //Clear all positions
    for i:=FPositions.count-1 downto 0 do begin
        p:=PPoint(FPositions.objects[i]);
        FPositions.delete(i);
        dispose(p);
    end;
end;

constructor TXPShellListView.Create(AOwner: TComponent);
var
    icondir: string;
begin
  inherited;
  //Initialize variables
  FIconLocationsKey:='';
  FOnGetSystemImage:=nil;
  FOnGetSystemCaption:=nil;
  FSystemIcons:=0;
  FPositions:=TStringList.create;
  FDirectoryMonitor:=TXPDirectoryMonitor.create(nil);
  FDirectory:='';
  FDirectoryMonitor.OnDirectoryModified:=updatepath;

  //Add system images
  ImageList:=TXPImageList.create(nil);
  icondir:=getSystemInfo(XP_NORMAL_SIZE_ICON_DIR);
  ImageList.add(icondir+'/noicon.png');
  ImageList.add(icondir+'/folder.png');
  ImageList.add(icondir+'/mydocuments.png');
  ImageList.add(icondir+'/mycomputer.png');
  ImageList.add(icondir+'/myhome.png');
  ImageList.add(icondir+'/mynetworkplaces.png');
  ImageList.add(icondir+'/recyclebin_empty.png');
  ImageList.add(icondir+'/recyclebin_full.png');

  //Setup events
  { TODO : Add virtual handlers }
  OnGetImageIndex:=doongetimage;
  OnGetItemCaption:=doongetitemcaption;
  OnGetItemPos:=doongetitempos;
  OnsetItemPos:=doonsetitempos;
end;

procedure TXPShellListView.dblclick;
var
    f: PFileInfo;
    filename: string;
    path: string;
    command: string;
    ini: TIniFile;
begin
  inherited;
  //Process double click
  if SelectedItem<>-1 then begin
        if SelectedItem>=SystemIcons then begin
            f:=FDirectoryMonitor.Directory.Files(SelectedItem-fsystemicons);
            filename:=f^.SR.Name;
            if ((extractfileext(filename))='.lnk') then begin
                path:=f^.SR.PathOnly+filename;
                ini:=TIniFile.Create(path);
                try
                    command:=ini.ReadString('Shortcut','Command','');
                finally
                    ini.free;
                end;

                { TODO : Add a common way to execute apps }
                if (command<>'') then libc.system(PChar(command+' &'));
            end;
        end;
  end;
end;

procedure TXPShellListView.deleteLocation(const iconName: string);
var
    reg: TRegistry;
begin
    //Delete an icon location from the registry
    if (FIconLocationsKey<>'') then begin
        reg:=TRegistry.create;
        try
            reg.RootKey:=HKEY_CURRENT_USER;
            if reg.openkey(FIconLocationsKey+'/'+iconName, true) then begin
                reg.DeleteKey;
            end;
        finally
            reg.free;
        end;
    end;
end;

destructor TXPShellListView.Destroy;
begin
  clearpositions;
  FPositions.free;
  ImageList.free;
  FDirectoryMonitor.free; 
  inherited;
end;

function TXPShellListView.doongetimage(Sender: TObject;
  const itemindex: integer): integer;
var
    f: PFileInfo;
    attr: integer;
begin
    //Returns an image index
    if (itemindex<fsystemicons) then begin
        result:=FOnGetSystemImage(self,itemindex);
    end
    else begin
        //Returns a file, folder or an image index
        f:=FDirectoryMonitor.Directory.Files(itemindex-fsystemicons);
        attr:=f^.SR.Attr;

        if (faDirectory and attr)=faDirectory then begin
            result:=1;
        end
        else result:=f^.ImageIndex;
    end;
end;

function TXPShellListView.doongetitemcaption(Sender: TObject;
  const itemindex: integer): string;
var
    f: PFileInfo;
    path: string;
    ini: TIniFile;
begin
    //Returns a caption for an intem
    if (itemindex<fsystemicons) then begin
        result:=FOnGetSystemCaption(sender, itemindex);
    end
    else begin
        f:=FDirectoryMonitor.Directory.Files(itemindex-fsystemicons);
        result:=f^.SR.Name;
        //Process an lnk file
        { TODO : Create a wrapper over a lnk file }
        if ((extractfileext(result))='.lnk') then begin
            path:=f^.SR.PathOnly+result;
            ini:=TIniFile.Create(path);
            try
                result:=ini.ReadString('Shortcut','Caption',changefileext(result,''));
            finally
                ini.free;
            end;
        end;
    end;
end;

procedure TXPShellListView.doongetitempos(Sender: TObject;
  const itemindex: integer; var pos: TPoint);
var
    p: PPoint;
    st: string;
    index:integer;
begin
    //Returns the position of an item
    st:=doongetitemcaption(self,itemindex);

    index:=FPositions.IndexOf(st);
    p:=PPoint(FPositions.objects[index]);

    pos.x:=p^.x;
    pos.y:=p^.y;
end;

procedure TXPShellListView.doonsetitempos(Sender: TObject;
  const itemindex: integer; pos: TPoint);
var
    p: PPoint;
    st: string;
    index:integer;
begin
    //Sets the position of an item
    st:=doongetitemcaption(self,itemindex);

    index:=FPositions.IndexOf(st);
    p:=PPoint(FPositions.objects[index]);
    p^.x:=pos.x;
    p^.y:=pos.y;

    //Store the location on the registry
    saveLocation(st,pos);
end;

function TXPShellListView.loadLocation(const iconName: string;
  var pos: TPoint):boolean;
var
    reg: TRegistry;
begin
    //Loads an icon location from the registry
    result:=false;
    if (FIconLocationsKey<>'') then begin
        reg:=TRegistry.create;
        try
            reg.RootKey:=HKEY_CURRENT_USER;
            if reg.openkey(FIconLocationsKey+'/'+iconName, false) then begin
                pos.x:=reg.Readinteger('Left');
                pos.y:=reg.Readinteger('Top');
                result:=true;
            end;
        finally
            reg.free;
        end;
    end;
end;

procedure TXPShellListView.saveLocation(const iconName: string;
  pos: TPoint);
var
    reg: TRegistry;
begin
    //Stores an icon location to the registry
    if (FIconLocationsKey<>'') then begin
        reg:=TRegistry.create;
        try
            reg.RootKey:=HKEY_CURRENT_USER;
            if reg.openkey(FIconLocationsKey+'/'+iconName, true) then begin
                reg.Writeinteger('Left',pos.x);
                reg.Writeinteger('Top',pos.y);
            end;
        finally
            reg.free;
        end;
    end;
end;

procedure TXPShellListView.SetDirectory(const Value: string);
begin
    //Sets the directory to browse
    if (value<>FDirectory) then begin
        FDirectory := Value;
        updatePath(nil);
    end;
end;

procedure TXPShellListView.SetIconLocationsKey(const Value: string);
begin
    //Registry key to store icon locations
    FIconLocationsKey := Value;
end;

procedure TXPShellListView.SetOnGetSystemCaption(const Value: TXPListViewGetItemCaptionEvent);
begin
  FOnGetSystemCaption := Value;
end;

procedure TXPShellListView.SetOnGetSystemImage(const Value: TXPListViewGetImageIndexEvent);
begin
  FOnGetSystemImage := Value;
end;

procedure TXPShellListView.SetSystemIcons(const Value: integer);
begin
  FSystemIcons := Value;
end;

procedure TXPShellListView.updatePath(sender:TObject);
var
    i: integer;
    p: PPoint;
    ap: TPoint;
    st: string;
    f: PFileInfo;
    fname: string;
    ini: TIniFile;
    image: string;
    store: TStringList;
    idx: integer;
begin
    //Updates the path and refreshes item list
    if FDirectory<>'' then begin
        FDirectoryMonitor.Directory.Location:=FDirectory;
        ItemCount:=FDirectoryMonitor.Directory.Count+FSystemIcons;
        FPositions.sorted:=true;

        //Iterates through items
        store:=TStringList.create;
        try
        for i:=0 to itemcount-1 do begin
            //Get the caption
            st:=doongetitemcaption(self, i);
            //If it doesn't exists
            idx:=FPositions.indexof(st);
            if (idx=-1) then begin
                //If it's not a system icon
                if (i>=fsystemicons) then begin
                    //Loads an icon for it
                    f:=FDirectoryMonitor.Directory.Files(i-fsystemicons);
                    fname:=f^.SR.Name;
                    f.ImageIndex:=0;

                    if (ansilowercase(extractfileext(fname))='.lnk') then begin
                            { TODO : Create a wrapper over a lnk file }
                            ini:=TIniFile.create(f^.SR.PathOnly+fname);
                            try
                                image:=ini.ReadString('Shortcut','Icon','');
                                if (image<>'') then f.imageindex:=ImageList.Add(image);
                            finally
                                ini.free;
                            end;
                    end;
                end;

                //Creates a point
                p:=new(PPoint);
                //Initializes a default position
                ap:=getdefaultposition(i);

                //Try to get the position from the registry
                loadLocation(st,ap);
                p^.x:=ap.x;
                p^.y:=ap.y;
                idx:=FPositions.addobject(st,TObject(p));
                store.add(st);
            end
            else begin
                store.add(st);
            end;
        end;

        store.sorted:=true;
        i:=0;
        while (i<=store.count-1) do begin
                if (FPositions[i]=store[i]) then inc(i)
                else begin
                        deletelocation(FPositions[i]);
                        FPositions.delete(i);
                end;
        end;

        while (i<=FPositions.count-1) do begin
                deletelocation(FPositions[i]);
                FPositions.delete(i);
                inc(i);
        end;

        if (SelectedItem>=itemcount) then begin
                SelectedItem:=itemcount-1;
        end;

        finally
                store.free;
        end;

        //Redraw full contents
        redraw(-1,true);
    end;
end;

end.
