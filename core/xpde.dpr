program xpde;

uses
  QForms,
  main in 'main.pas' {Mainform},
  uXPDesktop in 'uXPDesktop.pas',
  uXPShellListView in '../components/XPShellListView/uXPShellListView.pas',
  uXPListView in '../components/XPListView/uXPListView.pas',
  uGraphics in '../common/uGraphics.pas',
  uXPImageList in '../components/XPImageList/uXPImageList.pas',
  uXPPNG in '../common/uXPPNG.pas',
  uCommon in '../common/uCommon.pas',
  QThemed in '../common/QTheming/QThemed.pas',
  QThemeSrvLinux in '../common/QTheming/QThemeSrvLinux.pas',
  uRegistry in '../components/XPRegistry/uRegistry.pas',
  uRegLib in '../components/XPRegistry/uRegLib.pas',
  uResample in '../common/uResample.pas',
  uXPDirectoryMonitor in '../components/XPDirectoryMonitor/uXPDirectoryMonitor.pas',
  uXPIPC in '../common/uXPIPC.pas',
  uTaskbar in 'uTaskbar.pas' {taskbar},
  uWMConsts in 'uWMConsts.pas',
  uXPStartButton in 'uXPStartButton.pas',
  uXPSysTray in 'uXPSysTray.pas',
  uXPTaskband in 'uXPTaskband.pas',
  uWindowManager in 'uWindowManager.pas',
  uActiveTasks in 'uActiveTasks.pas' {ActiveTasksDlg},
  uXPStyledFrame in 'uXPStyledFrame.pas' {StyledFrame},
  uXPStartMenu in 'uXPStartMenu.pas' {StartMenu};

{$R *.res}

begin
  Application.Initialize;
  XPwindowmanager.install;
  Application.CreateForm(TMainform, Mainform);
  Application.CreateForm(Ttaskbar, taskbar);
  Application.Run;
end.
