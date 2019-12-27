program desktop_properties;

uses
  QForms,
  QThemed in '../../common/QTheming/QThemed.pas',
  uDisplayProperties in 'uDisplayProperties.pas' {DisplayPropertiesDlg},
  uXPColorSelector in '../../components/XPColorSelect/uXPColorSelector.pas',
  uXPColorDialog in '../../components/XPColorSelect/uXPColorDialog.pas' {XPColorDialog},
  uRegistry in '../../components/XPRegistry/uRegistry.pas',
  uRegLib in '../../components/XPRegistry/uRegLib.pas',
  uXPIPC in '../../common/uXPIPC.pas',
  QThemeSrvLinux in '../../common/QTheming/QThemeSrvLinux.pas';

{$R *.res}

begin

  Application.Initialize;
  Application.CreateForm(TDisplayPropertiesDlg, DisplayPropertiesDlg);
  Application.CreateForm(TXPColorDialog, XPColorDialog);
  Application.Run;
end.
