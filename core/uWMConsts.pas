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

unit uWMConsts;

interface

uses
    QGraphics, QMenus, XLib;

const
    { TODO : Extract these values from the system metrics }
    ICON_WIDTH=32;
    ICON_HEIGHT=32;
    MINI_ICON_WIDTH=16;
    MINI_ICON_HEIGHT=16;

    maxAtoms=78;

    atomsSupported=43;

    atom_net_wm_name =0;
    atom_wm_protocols =1;
    atom_wm_take_focus =2;
    atom_wm_delete_window =3;
    atom_wm_state =4;
    atom_net_close_window =5;
    atom_net_wm_state =6;
    atom_motif_wm_hints =7;
    atom_net_wm_state_shaded =8;
    atom_net_wm_state_maximized_horz =9;
    atom_net_wm_state_maximized_vert =10;
    atom_net_wm_desktop =11;
    atom_net_number_of_desktops =12;
    atom_wm_change_state =13;
    atom_sm_client_id =14;
    atom_wm_client_leader =15;
    atom_wm_window_role =16;
    atom_net_current_desktop =17;
    atom_net_supporting_wm_check =18;
    atom_net_supported =19;
    atom_net_wm_window_type =20;
    atom_net_wm_window_type_desktop =21;
    atom_net_wm_window_type_dock =22;
    atom_net_wm_window_type_toolbar =23;
    atom_net_wm_window_type_menu =24;
    atom_net_wm_window_type_dialog =25;
    atom_net_wm_window_type_normal =26;
    atom_net_wm_state_modal =27;
    atom_net_client_list =28;
    atom_net_client_list_stacking =29;
    atom_net_wm_state_skip_taskbar =30;
    atom_net_wm_state_skip_pager =31;
    atom_win_workspace =32;
    atom_win_layer =33;
    atom_win_protocols =34;
    atom_win_supporting_wm_check =35;
    atom_net_wm_icon_name =36;
    atom_net_wm_icon =37;
    atom_net_wm_icon_geometry =38;
    atom_utf8_string =39;
    atom_wm_icon_size =40;
    atom_kwm_win_icon =41;
    atom_net_wm_moveresize =42;
    atom_net_active_window =43;

    atom_metacity_restart_message =44;
    atom_net_wm_strut =45;
    atom_win_hints =46;
    atom_metacity_reload_theme_message =47;
    atom_metacity_set_keybindings_message =48;
    atom_net_wm_state_hidden =49;
    atom_net_wm_window_type_utility =50;
    atom_net_wm_window_type_splashscreen =51;
    atom_net_wm_state_fullscreen =52;
    atom_net_wm_ping =53;
    atom_net_wm_pid =54;
    atom_wm_client_machine =55;
    atom_net_workarea =56;
    atom_net_showing_desktop =57;
    atom_net_desktop_layout =58;
    atom_manager =59;
    atom_targets =60;
    atom_multiple =61;
    atom_timestamp =62;
    atom_version =63;
    atom_atom_pair =64;
    atom_net_desktop_names =65;
    atom_net_wm_allowed_actions =66;
    atom_net_wm_action_move =67;
    atom_net_wm_action_resize =68;
    atom_net_wm_action_shade =69;
    atom_net_wm_action_stick =70;
    atom_net_wm_action_maximize_horz =71;
    atom_net_wm_action_maximize_vert =72;
    atom_net_wm_action_change_desktop =73;
    atom_net_wm_action_close =74;
    atom_net_wm_state_above =75;
    atom_net_wm_state_below =76;
    atom_KDE_NET_WM_SYSTEM_TRAY_WINDOW_FOR=77;
    atom_BY_PASS_WM=78;

type
    { TODO : Make this interface an standard }    
    IWMClient=interface
    ['{8225D62E-CEE8-D611-927C-000244219999}']
        procedure focus;
        procedure minimize;
        procedure maximize;
        procedure beginresize;
        procedure endresize;        
        procedure restore;
        procedure close;
        procedure map;
        procedure bringtofront;
        procedure updateactivestate;
        function isactive:boolean;
        procedure activate(restore:boolean=true);
        function getTitle: widestring;
        function getBitmap: TBitmap;
        function getSystemMenu: TPopupMenu;
        function getWindow: Window;
    end;    


implementation

end.
