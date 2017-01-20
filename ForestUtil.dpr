{ TODO :
Speed up Query type analyse. Well... I don't know, how! May be, push it into thread?
}

program ForestUtil;

uses
  Windows,
  Forms,
  UI in 'UI.pas' {frmUI},
  Data in 'Data.pas' {dmData: TDataModule},
  Dictionary in 'Dictionary.pas',
  SQLQuery in 'SQLQuery.pas',
  Validator in 'Validator.pas',
  ForestConsts in 'ForestConsts.pas',
  Edit in 'Edit.pas' {frmEdit},
  Settings in 'Settings.pas' {frmSettings},
  Dicts in 'Dicts.pas' {frmDicts},
  NsUtils in 'NsUtils.pas',
  DBScript in 'DBScript.pas',
  ForestTypes in 'ForestTypes.pas',
  About in 'About.pas' {AboutBox},
  AskForestry in 'AskForestry.pas' {frmAskForestry},
  Catalogs in 'Catalogs.pas' {frmCatalogs},
  ReportManager in 'ReportManager.pas' {dmReportManager: TDataModule};

procedure SwitchToThisWindow(h1: hWnd; x: bool); stdcall;
  external user32 Name 'SwitchToThisWindow';

{$R *.res}

const
  MutexName = '{4B634202-CA07-44D9-9864-1DD021BF4B9F}';
  S_USAGE: AnsiString = 'Использование:' + #13#10 +
    'ForestUtil.exe -file FileName.xls -table SheetName -validate';

var
  Mutex: THandle;
  h: HWND;

begin
  Mutex := CreateMutex(nil, False, MutexName);

  if Mutex = 0 then
    MessageBox(0, 'Невозможно создать мьютекс', 'Error', MB_OK or MB_ICONSTOP)

  else if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    h := FindWindow('TfrmUI', 'ForestUtil');
    h := GetWindow(h, GW_OWNER);
    OpenIcon(h);
    ShowWindow(h, SW_SHOWNORMAL);
    BringWindowToTop(h);
    SetForegroundWindow(h);
  end

  else
  begin
    Application.Initialize;
    Application.Title := 'ForestUtil';
    Application.HelpFile := 'ForestUtil.hlp';
    Application.CreateForm(TdmData, dmData);
    Application.CreateForm(TdmReportManager, dmReportManager);
    Application.CreateForm(TfrmUI, frmUI);
    Application.CreateForm(TfrmEdit, frmEdit);
    Application.CreateForm(TfrmSettings, frmSettings);
    Application.CreateForm(TfrmDicts, frmDicts);
    Application.CreateForm(TAboutBox, AboutBox);
    Application.CreateForm(TfrmAskForestry, frmAskForestry);
    Application.CreateForm(TfrmCatalogs, frmCatalogs);

    if HasCmdParam('-help') then
      MessageBox(0, PChar(S_USAGE), 'Help', MB_OK or MB_ICONSTOP);

    if HasCmdParam('-file') then
    begin
      frmUI.OpenFile(ParamStr(2));

      if HasCmdParam('-table') then
      begin
        frmUI.OpenTable(ParamStr(4) + '$');

        if HasCmdParam('-validate') then
          frmUI.actValidateTableExecute(Application);
      end;
    end;

    Application.Run;
    CloseHandle(Mutex);
  end;
end.

