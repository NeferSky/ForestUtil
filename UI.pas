unit UI;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Grids, DBGrids, ComCtrls, Menus, DB,
  ActnList, ImgList, SQLQuery, XPMan, ScrolledMemo, Edit, Buttons,
  ForestTypes, ADODB, ReportManager;

type
  TReportMenuItem = class(TMenuItem)
  private
    FReportName: AnsiString;
  public
    property ReportName: AnsiString read FReportName write FReportName;
  end;
  
type
  TfrmUI = class(TForm)
    // Invisible components
    manXP: TXPManifest;
    ilUIImages: TImageList;
    tmrTimer: TTimer;
    odlgOpenFile: TOpenDialog;
    odlgOpenQuery: TOpenDialog;
    sdlgSaveQuery: TSaveDialog;
    dsFile: TDataSource;
    dsQuery: TDataSource;

    // Actions
    actlUIActions: TActionList;
    actOpenFile: TAction;
    actPasteSelectTemplate: TAction;
    actPasteInsertTemplate: TAction;
    actPasteUpdateTemplate: TAction;
    actPasteDeleteTemplate: TAction;
    actExecuteQuery: TAction;
    actValidateTable: TAction;
    actSettings: TAction;
    actExit: TAction;
    actRestore: TAction;
    actNextQuery: TAction;
    actPrevQuery: TAction;
    actDictSettings: TAction;
    actSaveQueryResult: TAction;
    actBlowCoffee: TAction;
    actSaveLog: TAction;
    actSaveQuery: TAction;
    actOpenQuery: TAction;
    actClearQueryText: TAction;
    actHelp: TAction;
    actAbout: TAction;
    actCreateScript: TAction;
    actPrevSkippedRec: TAction;
    actNextSkippedRec: TAction;
    actValidateRecord: TAction;

    // Menu
    mmnuMain: TMainMenu;
    mnuMain: TMenuItem;
    mnuBlowCoffee: TMenuItem;
    mnuExit: TMenuItem;
    mnuFile: TMenuItem;
    mnuOpenFile: TMenuItem;
    mnuValidateFile: TMenuItem;
    mnuCreateScript: TMenuItem;
    mnuExecuteQuery: TMenuItem;
    mnuOpenQuery: TMenuItem;
    mnuSaveQuery: TMenuItem;
    mnuSaveQueryResult: TMenuItem;
    mnuView: TMenuItem;
    mnuCompressColumns: TMenuItem;
    mnuAllSettings: TMenuItem;
    mnuParams: TMenuItem;
    mnuDictionaryEdit: TMenuItem;
    mnuHelp: TMenuItem;
    mnuShowHelp: TMenuItem;
    mnuAbout: TMenuItem;

    pmnuMain: TPopupMenu;

    pmnuTray: TPopupMenu;
    mnuRestore: TMenuItem;

    // Visible components
    pcPages: TPageControl;
    tsFileWorkspace: TTabSheet;
    splHint: TSplitter;
    pnlHint: TPanel;
    lblHint: TLabel;

    pnlFileWorkspace: TPanel;
    grdFile: TDBGrid;
    splLog: TSplitter;
    memLog: TScrolledMemo;

    splFileActions: TSplitter;
    pnlFileActions: TPanel;
    lblSelectSheet: TLabel;
    cmbFileSheet: TComboBox;

    gbxStatus: TGroupBox;
    lblStatus: TLabel;
    lblCurrentRecordNo: TLabel;
    lblSlash: TLabel;
    lblRecordsCount: TLabel;

    btnSaveLog: TButton;

    grbLogDetails: TGroupBox;
    chbMathErrors: TCheckBox;
    chbDuplicates: TCheckBox;
    chbRelationErrors: TCheckBox;
    chbDictReplaces: TCheckBox;
    chbEmptyRecords: TCheckBox;
    chbPrevReportSum: TCheckBox;

    gbxSkippedRecs: TGroupBox;
    sbtnPrevRec: TSpeedButton;
    sbtnNextRec: TSpeedButton;
    btnValidateRecord: TButton;

    btnMathValidate: TButton;
    btnCreateScript: TButton;
    pbFileProcess: TProgressBar;

    tsQueryWorkspace: TTabSheet;
    pnlQueryWorkspace: TPanel;
    memQueryText: TScrolledMemo;
    splQueryResult: TSplitter;
    grdQueryResult: TDBGrid;

    splQueryActions: TSplitter;
    pnlQueryActions: TPanel;
    gbxQueryHistory: TGroupBox;
    lblQueriesCount: TLabel;
    sbtnPrevQuery: TSpeedButton;
    sbtnNextQuery: TSpeedButton;
    rgrQueryType: TRadioGroup;
    gbxTemplate: TGroupBox;
    btnSelectTemplate: TButton;
    btnInsertTemplate: TButton;
    btnUpdateTemplate: TButton;
    btnDeleteTemplate: TButton;
    btnClearQueryText: TButton;
    btnSaveQueryResult: TButton;

    gbxRecordsFetched: TGroupBox;
    lblRecordsFetched: TLabel;

    btnExecuteQuery: TButton;
    pbQueryProcess: TProgressBar;

    sbStatus: TStatusBar;
    actCatalogSettings: TAction;
    mnuCatalogEdit: TMenuItem;
    mnuReports: TMenuItem;
    actRepPrevReport: TAction;

    // Components Events
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure QueryKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure OpenFile(const FileName: AnsiString);
    procedure OpenTable(const TableName: AnsiString);
    procedure lblHintClick(Sender: TObject);
    procedure pcPagesChange(Sender: TObject);
    procedure cmbFileSheetSelect(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure pnlQueryActionsResize(Sender: TObject);
    procedure pnlFileActionsResize(Sender: TObject);
    procedure grdFileTitleClick(Column: TColumn);
    procedure splFileActionsMoved(Sender: TObject);
    procedure mnuCompressColumnsClick(Sender: TObject);
    procedure chbLogDetailClick(Sender: TObject);
    procedure MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos:
      TPoint; var Handled: Boolean);
    procedure MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos:
      TPoint; var Handled: Boolean);
    procedure grdFileDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol:
      Integer; Column: TColumn; State: TGridDrawState);
    // Actions
    procedure actOpenFileExecute(Sender: TObject);
    procedure actPasteSelectTemplateExecute(Sender: TObject);
    procedure actPasteInsertTemplateExecute(Sender: TObject);
    procedure actPasteUpdateTemplateExecute(Sender: TObject);
    procedure actPasteDeleteTemplateExecute(Sender: TObject);
    procedure actExecuteQueryExecute(Sender: TObject);
    procedure actValidateTableExecute(Sender: TObject);
    procedure actRestoreExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actNextQueryExecute(Sender: TObject);
    procedure actPrevQueryExecute(Sender: TObject);
    procedure actDictSettingsExecute(Sender: TObject);
    procedure actSaveQueryResultExecute(Sender: TObject);
    procedure tmrTimerTimer(Sender: TObject);
    procedure actBlowCoffeeExecute(Sender: TObject);
    procedure actSaveLogExecute(Sender: TObject);
    procedure actOpenQueryExecute(Sender: TObject);
    procedure actSaveQueryExecute(Sender: TObject);
    procedure actClearQueryTextExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure actCreateScriptExecute(Sender: TObject);
    procedure actPrevSkippedRecExecute(Sender: TObject);
    procedure actNextSkippedRecExecute(Sender: TObject);
    procedure actValidateRecordExecute(Sender: TObject);
    procedure ImageClick(Sender: TObject);
    procedure actCatalogSettingsExecute(Sender: TObject);
    procedure actRepPrevReportExecute(Sender: TObject);
  private
    { Private declarations }
    FSQLQueries: TSQLQueries;
    FSelectedColumn: Integer;
    FCompressColumns: Boolean;
    FReportMgr: TdmReportManager;
    //--
    procedure ReadParams;
    procedure WriteParams;
    procedure ReadSettings;
    procedure WriteSettings;
    procedure PositionTable;
    procedure ReadReports;
    procedure EnableFileControls(Active: Boolean);
    procedure RecalcFileGridCols;
    procedure InitFileProcessControls;
    procedure ResetFileProcessControls;
    procedure ShowQueriesCount;
    function CheckQueryType: TQueryType;
    procedure ShowQueryResultGrid;
    procedure ControlWindow(var Msg: TMessage); message WM_SYSCOMMAND;
    procedure IconMouse(var Msg: TMessage); message WM_USER + 1;
    procedure Tray(n: Integer; Icon: TIcon);
    procedure ValidationLog(Msg: AnsiString);
    procedure StepProcess(CurrentIteration: Integer);
    procedure ReportMenuClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmUI: TfrmUI;

implementation

uses
  ShellAPI, Registry, ForestConsts, IniFiles, NsUtils, Data, Settings, Dicts,
  About, AskForestry, Catalogs;

{$R *.dfm}
{$R 'res\Coffee.res'}

//---------------------------------------------------------------------------
{ TfrmUI }

procedure TfrmUI.actAboutExecute(Sender: TObject);
begin
  AboutBox.ProductName.Caption := Application.Title;
  AboutBox.Version.Caption := Format(S_VERSION,
    [GetFullFileVersion(Application.ExeName)]);
  AboutBox.Copyright.Caption := S_COPYRIGHT;
  AboutBox.Comments.Caption := S_COMMENTS;
  AboutBox.ShowModal();
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actBlowCoffeeExecute(Sender: TObject);
var
  Img: TImage;
  Creator: TWinControl;

begin
  if pcPages.ActivePageIndex = 0 then
    Creator := grdFile
  else
    Creator := memQueryText;

  Img := TImage.Create(Creator);
  Img.Align := AlClient;
  Img.Picture.Bitmap.Width := Creator.ClientWidth;
  Img.Picture.Bitmap.Height := Creator.ClientHeight;
  Img.Stretch := True;
  Img.OnClick := ImageClick;
  Img.Parent := Creator;
  Img.Picture.Bitmap.Handle := LoadBitmap(hInstance, 'COFFEE');
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actCatalogSettingsExecute(Sender: TObject);
begin
  frmCatalogs.ShowModal();
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actClearQueryTextExecute(Sender: TObject);
begin
  memQueryText.Clear();
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actCreateScriptExecute(Sender: TObject);
begin
  if vrSkip in dmData.ValidationResult then
    ShowMessage(S_LOG_SKIPPED_LINES)
  else
  begin
    memQueryText.Text := dmData.Script;
    rgrQueryType.ItemIndex := 1;
    pcPages.SelectNextPage(True);
  end;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actDictSettingsExecute(Sender: TObject);
begin
  frmDicts.ShowModal();
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actExecuteQueryExecute(Sender: TObject);
var
  QueryType: TQueryType;

begin
  if dmData.InProgress then
  begin
    ShowMessage(S_IN_PROGRESS);
    Exit;
  end;

  if Trim(memQueryText.Text) = '' then
    Exit;

  case rgrQueryType.ItemIndex of
    0: QueryType := qtSelect
  else
    QueryType := qtCommand;
  end;

  // If I not sure that QueryType is correct - I'll ask user about it
  if QueryType <> CheckQueryType() then
    if MessageDlg(S_QUERY_TYPE_CONFIRM, mtConfirmation, mbOkCancel, 0) <> mrOk then
      Exit;

  case QueryType of
    qtSelect:
      begin
        dmData.GetQueryResult(memQueryText.Text);
        ShowQueryResultGrid();
        btnSaveQueryResult.Enabled := True;
        lblRecordsFetched.Caption := IntToStr(dmData.GetQueryRecordsCount);
        gbxRecordsFetched.Visible := True;
        btnSaveQueryResult.Enabled := dmData.GetQueryRecordsCount > 0;
      end;
    qtCommand:
      begin
        dmData.ExecuteQuery(memQueryText.Text);
        gbxRecordsFetched.Visible := False;
      end;
  end;

  FSQLQueries.Add(memQueryText.Text);
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actExitExecute(Sender: TObject);
begin
  Close();
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actHelpExecute(Sender: TObject);
begin
  RunApp('hh.exe ForestUtil.chm', GetAppPath());
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actNextQueryExecute(Sender: TObject);
var
  S: AnsiString;

begin
  S := FSQLQueries.NextQuery();

  if Trim(S) <> '' then
  begin
    memQueryText.Clear();
    memQueryText.Text := S;
    ShowQueriesCount();
  end;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actNextSkippedRecExecute(Sender: TObject);
begin
  dmData.MoveNextSkipped;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actOpenFileExecute(Sender: TObject);
begin
  if odlgOpenFile.Execute() then
    OpenFile(odlgOpenFile.FileName);
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actOpenQueryExecute(Sender: TObject);
begin
  if odlgOpenQuery.Execute() then
  begin
    memQueryText.Clear();
    memQueryText.Lines.LoadFromFile(odlgOpenQuery.FileName);
  end;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actPasteDeleteTemplateExecute(Sender: TObject);
begin
  memQueryText.SetFocus();
  memQueryText.SetSelTextBuf(PAnsiChar(SQLDeleteTemplate));
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actPasteInsertTemplateExecute(Sender: TObject);
begin
  memQueryText.SetFocus();
  memQueryText.SetSelTextBuf(PAnsiChar(SQLInsertTemplate));
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actPasteSelectTemplateExecute(Sender: TObject);
begin
  memQueryText.SetFocus();
  memQueryText.SetSelTextBuf(PAnsiChar(SQLSelectTemplate));
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actPasteUpdateTemplateExecute(Sender: TObject);
begin
  memQueryText.SetFocus();
  memQueryText.SetSelTextBuf(PAnsiChar(SQLUpdateTemplate));
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actPrevQueryExecute(Sender: TObject);
var
  S: AnsiString;

begin
  S := FSQLQueries.PrevQuery();

  if Trim(S) <> '' then
  begin
    memQueryText.Clear();
    memQueryText.Text := S;
    ShowQueriesCount();
  end;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actPrevSkippedRecExecute(Sender: TObject);
begin
  dmData.MovePrevSkipped;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actRepPrevReportExecute(Sender: TObject);
begin
//  FReportMgr.ReportQueryResult(const DataSet: TDataSet; const TableName: AnsiString);
end;
  
//---------------------------------------------------------------------------

procedure TfrmUI.actRestoreExecute(Sender: TObject);
begin
  Tray(2, Application.Icon);
  ShowWindow(Application.Handle, SW_SHOW);
  ShowWindow(Handle, SW_SHOW);
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actSaveLogExecute(Sender: TObject);
begin
  memLog.Lines.SaveToFile(GetAppPath() + S_LOG_FILE_NAME);
  RunApp('notepad.exe ' + GetAppPath() + S_LOG_FILE_NAME);
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actSaveQueryExecute(Sender: TObject);
begin
  if sdlgSaveQuery.Execute() then
    memQueryText.Lines.SaveToFile(sdlgSaveQuery.FileName);
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actSaveQueryResultExecute(Sender: TObject);
begin
  FReportMgr.ReportQueryResult(TDataSet(dmData.qrySelect), S_DB_TABLE_NAME);
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actSettingsExecute(Sender: TObject);
begin
  frmSettings.ShowModal();
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actValidateRecordExecute(Sender: TObject);
begin
  dmData.ValidateRecord(frmAskForestry.RegionID, frmAskForestry.ForestryID,
    frmAskForestry.ReportYear, frmAskForestry.ReportQuarter);
  gbxSkippedRecs.Enabled := Length(dmData.SkippedRecs) > 0
end;

//---------------------------------------------------------------------------

procedure TfrmUI.actValidateTableExecute(Sender: TObject);
begin
  if dmData.InProgress then
    Exit;

  memLog.Clear();
  ResetFileProcessControls();
  PositionTable();
  lblStatus.Caption := S_STATUS_PROCESSING;
  Application.ProcessMessages();

  dmData.ValidateTable(frmAskForestry.RegionID, frmAskForestry.ForestryID,
    frmAskForestry.ReportYear, frmAskForestry.ReportQuarter);

  if (vrMainInvalid in dmData.ValidationResult) and (ldMathErrors in
    dmData.LogDetails) then
    ShowMessage(S_LOG_MAIN_INVALID)
  else if (vrExtraInvalid in dmData.ValidationResult) and (ldMathErrors in
    dmData.LogDetails) then
    ShowMessage(S_LOG_EXTRA_INVALID)
  else if (vrDuplicateInvalid in dmData.ValidationResult) and (ldDuplicates in
    dmData.LogDetails) then
    ShowMessage(S_LOG_DUPLICATE_INVALID)
  else if (vrRelationInvalid in dmData.ValidationResult) and (ldRelationErrors in
    dmData.LogDetails) then
    ShowMessage(S_LOG_RELATION_INVALID)
  else if vrStop in dmData.ValidationResult then
    Application.ProcessMessages() // Do nothing
  else
    ShowMessage(S_LOG_SUCCESSFULLY);

  ResetFileProcessControls();
end;

//---------------------------------------------------------------------------

procedure TfrmUI.chbLogDetailClick(Sender: TObject);
var
  LogDetails: TLogDetails;

begin
  // A kind of magic - set property via local variable
  LogDetails := dmData.LogDetails;

  if (Sender as TCheckBox).Checked then
    case (Sender as TCheckBox).Tag of
      0: Include(LogDetails, ldMathErrors);
      1: Include(LogDetails, ldDuplicates);
      2: Include(LogDetails, ldRelationErrors);
      3: Include(LogDetails, ldDictReplaces);
      4: Include(LogDetails, ldEmptyRecords);
      5: Include(LogDetails, ldPrevReportSum);
    end
  else
    case (Sender as TCheckBox).Tag of
      0: Exclude(LogDetails, ldMathErrors);
      1: Exclude(LogDetails, ldDuplicates);
      2: Exclude(LogDetails, ldRelationErrors);
      3: Exclude(LogDetails, ldDictReplaces);
      4: Exclude(LogDetails, ldEmptyRecords);
      5: Exclude(LogDetails, ldPrevReportSum);
    end;

  dmData.LogDetails := LogDetails;
end;

//---------------------------------------------------------------------------

function TfrmUI.CheckQueryType: TQueryType;
var
  SQLText: AnsiString;
  WordCount, DotCommaCount: Integer;
  Braked1Count, Braked2Count, EqualCount, CommaCount: Integer;

begin
  Result := qtCommand;

  SQLText := memQueryText.Text;
  SQLText := DeleteLineBreaks(SQLText);
  SQLText := AnsiUpperCase(SQLText);

  // if 'SELECT' is first word
  if AnsiPos(S_FREE_SELECT, SQLText) = 1 then
  begin
    Result := qtSelect;
    Exit;
  end;

  WordCount := (Length(SQLText) - Length(StringReplace(SQLText,
    S_FREE_SELECT, '', [rfReplaceAll]))) div Length(S_FREE_SELECT);
  DotCommaCount := (Length(SQLText) - Length(StringReplace(SQLText,
    S_DOTCOMMA_SELECT, '', [rfReplaceAll]))) div Length(S_DOTCOMMA_SELECT);
  Braked1Count := (Length(SQLText) - Length(StringReplace(SQLText,
    S_BRACKED1_SELECT, '', [rfReplaceAll]))) div Length(S_BRACKED1_SELECT);
  Braked2Count := (Length(SQLText) - Length(StringReplace(SQLText,
    S_BRACKED2_SELECT, '', [rfReplaceAll]))) div Length(S_BRACKED2_SELECT);
  EqualCount := (Length(SQLText) - Length(StringReplace(SQLText,
    S_EQUAL_SELECT, '', [rfReplaceAll]))) div Length(S_EQUAL_SELECT);
  CommaCount := (Length(SQLText) - Length(StringReplace(SQLText,
    S_COMMA_SELECT, '', [rfReplaceAll]))) div Length(S_COMMA_SELECT);

  // Free SELECT more than (SELECT + )SELECT + =SELECT + ,SELECT
  if WordCount > (Braked1Count + Braked2Count + EqualCount + CommaCount) then
    Result := qtSelect;

  // Free SELECT the same count of (SELECT + )SELECT + =SELECT + ,SELECT
  if WordCount = (Braked1Count + Braked2Count + EqualCount + CommaCount) then
    Result := qtCommand;

  // There are ;SELECT
  if DotCommaCount > 0 then
    Result := qtSelect;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.cmbFileSheetSelect(Sender: TObject);
begin
  OpenTable(cmbFileSheet.Text);
end;

//---------------------------------------------------------------------------

procedure TfrmUI.ControlWindow(var Msg: TMessage);
begin
  if (Msg.WParam = SC_MINIMIZE) and TrayEnabled then
  begin
    Tray(1, Application.Icon);
    ShowWindow(Handle, SW_HIDE);
    ShowWindow(Application.Handle, SW_HIDE);
  end
  else
    inherited;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.EnableFileControls(Active: Boolean);
var
  I: Integer;

begin
  for I := 0 to pnlFileActions.ControlCount - 1 do
    pnlFileActions.Controls[I].Enabled := Active;

  gbxSkippedRecs.Enabled := False;
  btnSaveLog.Enabled := memLog.Visible;
  btnMathValidate.Enabled := False;
  btnCreateScript.Enabled := False;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

//---------------------------------------------------------------------------

procedure TfrmUI.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Self.Handle, True);
  ReadSettings();
  ReadParams();
  ReadReports();
  FSQLQueries := TSQLQueries.Create(frmUI);
  tmrTimerTimer(Sender);
  EnableFileControls(False);
  InitFileProcessControls();
  pcPagesChange(Sender);
  ShowQueriesCount();
  sbStatus.SimpleText := Application.Title + ' v.' +
    GetFullFileVersion(Application.ExeName);
  dmData.OnValidationLog := ValidationLog;
  dmData.OnProgress := StepProcess;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Self.Handle, False);
  WriteSettings();
  WriteParams();
  FSQLQueries.Free();
end;

//---------------------------------------------------------------------------

procedure TfrmUI.FormResize(Sender: TObject);
begin
  RecalcFileGridCols();
end;

//---------------------------------------------------------------------------

procedure TfrmUI.grdFileDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  SelectedFontStyle: TFontStyles;

begin
  if (grdFile.DataSource.DataSet.RecNo < dmData.FirstRecNo) or
    (grdFile.DataSource.DataSet.RecNo > dmData.LastRecNo) then
  begin
    grdFile.Canvas.Brush.Color := clSilver;
    grdFile.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;

  if dmData.SkippedRow(grdFile.DataSource.DataSet.RecNo) then
  begin
    grdFile.Canvas.Brush.Color := clYellow;
    grdFile.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;

  if dmData.InvalidRow(grdFile.DataSource.DataSet.RecNo) and (ldMathErrors in
    dmData.LogDetails) then
  begin
    grdFile.Canvas.Brush.Color := clRed;
    grdFile.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;

  // Selected grid row
  if gdSelected in State then
  begin
    SelectedFontStyle := grdFile.Canvas.Font.Style;
    Include(SelectedFontStyle, fsBold);
    grdFile.Canvas.Font.Style := SelectedFontStyle;
    grdFile.Canvas.Font.Color := clBlack;
    grdFile.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.grdFileTitleClick(Column: TColumn);
begin
  FSelectedColumn := Column.Index;
  RecalcFileGridCols();
end;

//---------------------------------------------------------------------------

procedure TfrmUI.IconMouse(var Msg: TMessage);
var
  P: TPoint;

begin
  GetCursorPos(P);

  case Msg.LParam of
    WM_LBUTTONUP, WM_LBUTTONDBLCLK:
      begin
        Tray(2, Application.Icon);
        ShowWindow(Application.Handle, SW_SHOW);
        ShowWindow(Handle, SW_SHOW);
      end;

    WM_RBUTTONUP:
      pmnuTray.Popup(P.X, P.Y);
  end;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.ImageClick(Sender: TObject);
begin
  Sender.Free();
end;

//---------------------------------------------------------------------------

procedure TfrmUI.InitFileProcessControls;
begin
  pcPages.ActivePageIndex := 0;
  splLog.Visible := False;
  memLog.Clear();
  memLog.Visible := False;
  lblCurrentRecordNo.Caption := '-';
  lblRecordsCount.Caption := '-';
  lblStatus.Caption := S_STATUS_OFFLINE;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.lblHintClick(Sender: TObject);
begin
  actOpenFile.Execute();
end;

//---------------------------------------------------------------------------

procedure TfrmUI.mnuCompressColumnsClick(Sender: TObject);
begin
  mnuCompressColumns.Checked := not mnuCompressColumns.Checked;
  FCompressColumns := mnuCompressColumns.Checked;
  RecalcFileGridCols();
end;

//---------------------------------------------------------------------------

procedure TfrmUI.MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    (Sender as TMemo).Font.Height := (Sender as TMemo).Font.Height + 1;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    (Sender as TMemo).Font.Height := (Sender as TMemo).Font.Height - 1;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.OpenFile(const FileName: AnsiString);
begin
  if dmData.InProgress then
    Exit;
  InitFileProcessControls();

  if dmData.OpenFile(FileName) then
  begin
    lblStatus.Caption := S_STATUS_WAITING_TABLE;
    lblHint.Caption := ExtractFileName(FileName);
    grdFile.Color := clAppWorkSpace;
    cmbFileSheet.Clear();
    cmbFileSheet.Items.Assign(dmData.TableList);
  end;

  EnableFileControls(True);
end;

//---------------------------------------------------------------------------

procedure TfrmUI.OpenTable(const TableName: AnsiString);
begin
  if dmData.OpenTable(TableName) then
  begin
    grdFile.Color := clWindow;
    FSelectedColumn := 0;
    RecalcFileGridCols();
    lblRecordsCount.Caption := IntToStr(dmData.GetFileRecordsCount());
    pbFileProcess.Max := dmData.GetFileRecordsCount();
    lblStatus.Caption := S_STATUS_READY;
    btnMathValidate.Enabled := AskFileForestry();
    btnCreateScript.Enabled := dmData.Script <> '';
  end;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.pcPagesChange(Sender: TObject);
var
  FilePageActive: Boolean;

begin
  FilePageActive := pcPages.ActivePageIndex = 0;

  // File page controls
  mnuValidateFile.Visible := FilePageActive;
  mnuValidateFile.Enabled := FilePageActive;
  mnuCreateScript.Visible := FilePageActive;
  mnuCreateScript.Enabled := FilePageActive;
  mnuOpenFile.Visible := FilePageActive;
  mnuOpenFile.Enabled := FilePageActive;
  mnuView.Visible := FilePageActive;
  mnuView.Enabled := FilePageActive;
  mnuDictionaryEdit.Visible := FilePageActive;
  mnuDictionaryEdit.Enabled := FilePageActive;

  // Query page controls
  mnuExecuteQuery.Visible := not FilePageActive;
  mnuExecuteQuery.Enabled := not FilePageActive;
  mnuOpenQuery.Visible := not FilePageActive;
  mnuOpenQuery.Enabled := not FilePageActive;
  mnuSaveQuery.Visible := not FilePageActive;
  mnuSaveQuery.Enabled := not FilePageActive;
  mnuSaveQueryResult.Visible := not FilePageActive;
  mnuSaveQueryResult.Enabled := dmData.mtCache.Active and (not FilePageActive);
  btnSaveQueryResult.Enabled := dmData.mtCache.Active and (not FilePageActive);
  btnExecuteQuery.Enabled := not FilePageActive;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.pnlFileActionsResize(Sender: TObject);
var
  Wid: Integer;

begin
  Wid := (gbxSkippedRecs.Width - 15) div 2;

  sbtnPrevRec.Width := Wid;
  sbtnNextRec.Width := Wid;

  sbtnNextRec.Left := gbxSkippedRecs.Width div 2;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.pnlQueryActionsResize(Sender: TObject);
var
  Wid: Integer;

begin
  Wid := (gbxQueryHistory.Width - 15) div 2;

  sbtnPrevQuery.Width := Wid;
  sbtnNextQuery.Width := Wid;

  sbtnNextQuery.Left := gbxQueryHistory.Width div 2;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.PositionTable;
begin
  dsFile.Enabled := False;
  lblStatus.Caption := S_STATUS_POSITION_TABLE;
  Application.ProcessMessages();

  dmData.PositionTable();

  dsFile.Enabled := True;
  lblStatus.Caption := S_STATUS_READY;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.QueryKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = 65) then // Oops, it has been broken
    memQueryText.SelectAll()
  else if (ssCtrl in Shift) and ((Key = VK_1) or (Key = VK_NUMPAD1)) then
    actPasteSelectTemplate.Execute()
  else if (ssCtrl in Shift) and ((Key = VK_2) or (Key = VK_NUMPAD2)) then
    actPasteInsertTemplate.Execute()
  else if (ssCtrl in Shift) and ((Key = VK_3) or (Key = VK_NUMPAD3)) then
    actPasteUpdateTemplate.Execute()
  else if (ssCtrl in Shift) and ((Key = VK_4) or (Key = VK_NUMPAD4)) then
    actPasteDeleteTemplate.Execute()
  else if (ssCtrl in Shift) and ((Key = VK_5) or (Key = VK_NUMPAD5)) then
    actClearQueryText.Execute()
  else if (ssCtrl in Shift) and (ssAlt in Shift) and (Key = VK_LEFT) then
    actPrevQuery.Execute()
  else if (ssCtrl in Shift) and (ssAlt in Shift) and (Key = VK_RIGHT) then
    actNextQuery.Execute();
end;

//---------------------------------------------------------------------------

procedure TfrmUI.ReadParams;
begin
  with TRegistry.Create() do
    try
      RootKey := HKEY_CURRENT_USER;
      OpenKeyReadOnly(S_REG_KEY);

      if ValueExists(S_FORM_LEFT) then
        frmUI.Left := ReadInteger(S_FORM_LEFT);

      if ValueExists(S_FORM_TOP) then
        frmUI.Top := ReadInteger(S_FORM_TOP);

      if ValueExists(S_FORM_HEIGHT) then
        frmUI.Height := ReadInteger(S_FORM_HEIGHT);

      if ValueExists(S_FORM_WIDTH) then
        frmUI.Width := ReadInteger(S_FORM_WIDTH);

      if ValueExists(S_COMPRESS_COLUMNS) then
      begin
        FCompressColumns := ReadBool(S_COMPRESS_COLUMNS);
        mnuCompressColumns.Checked := FCompressColumns;
      end;

      if ValueExists(S_MATH_ERRORS) then
        chbMathErrors.Checked := ReadBool(S_MATH_ERRORS);

      if ValueExists(S_DUPLICATES) then
        chbDuplicates.Checked := ReadBool(S_DUPLICATES);

      if ValueExists(S_RELATION_ERRORS) then
        chbRelationErrors.Checked := ReadBool(S_RELATION_ERRORS);

      if ValueExists(S_DICT_REPLACES) then
        chbDictReplaces.Checked := ReadBool(S_DICT_REPLACES);

      if ValueExists(S_EMPTY_RECORDS) then
        chbEmptyRecords.Checked := ReadBool(S_EMPTY_RECORDS);

      if ValueExists(S_PREV_REPORT_SUM) then
        chbPrevReportSum.Checked := ReadBool(S_PREV_REPORT_SUM);

    finally
      CloseKey();
      Free();
    end;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.ReadReports;
var
  I: Integer;
  mnuItem: TReportMenuItem;

begin
  for I := 0 to dmReportManager.ReportList.Count - 1 do
  begin
    mnuItem := TReportMenuItem.Create(mmnuMain);
    mnuReports.Add(mnuItem);
    mnuItem.Caption := TReport(dmReportManager.ReportList[I]).Caption;
    mnuItem.ReportName := TReport(dmReportManager.ReportList[I]).Name;
    mnuItem.ImageIndex := 18;
    mnuItem.OnClick := ReportMenuClick;
  end;

  mnuReports.Visible := dmReportManager.ReportList.Count > 0;
end;
      
//---------------------------------------------------------------------------

procedure TfrmUI.ReadSettings;
begin
  if not FileExists(ExtractFilePath(Application.ExeName) + S_SETTINGS_FILE_NAME)
    then
    Exit;

  with TIniFile.Create(ExtractFilePath(Application.ExeName) +
    S_SETTINGS_FILE_NAME) do
    try
      SQLSelectTemplate := ReadString(S_INI_TEMPLATES, S_SELECT_TEMPLATE,
        SQLSelectTemplate);
      SQLInsertTemplate := ReadString(S_INI_TEMPLATES, S_INSERT_TEMPLATE,
        SQLInsertTemplate);
      SQLUpdateTemplate := ReadString(S_INI_TEMPLATES, S_UPDATE_TEMPLATE,
        SQLUpdateTemplate);
      SQLDeleteTemplate := ReadString(S_INI_TEMPLATES, S_DELETE_TEMPLATE,
        SQLDeleteTemplate);
      TrayEnabled := ReadBool(S_INI_GUI, S_TRAY_ENABLED, TrayEnabled);

    finally
      Free();
    end;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.RecalcFileGridCols;
var
  I, SelectedColWidth, ColWidth: Integer;

begin
  if not grdFile.DataSource.DataSet.Active then
    Exit;

  if FCompressColumns then
  begin
    SelectedColWidth := (grdFile.ClientWidth - 30) div 10;
    ColWidth := (grdFile.ClientWidth - SelectedColWidth - 30) div
      (grdFile.Columns.Count - 1);
  end

  else
  begin
    SelectedColWidth := I_DEFAULT_COL_WIDTH;
    ColWidth := I_DEFAULT_COL_WIDTH;
  end;

  for I := 0 to grdFile.Columns.Count - 1 do
    if I = FSelectedColumn then
      grdFile.Columns.Items[I].Width := SelectedColWidth
    else
      grdFile.Columns.Items[I].Width := ColWidth;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.ReportMenuClick(Sender: TObject);
begin
  dmReportManager.DoReport(TReportMenuItem(Sender).ReportName);
end;
   
//---------------------------------------------------------------------------

procedure TfrmUI.ResetFileProcessControls;
begin
  pbFileProcess.Position := 0;
  lblCurrentRecordNo.Caption := '0';
  btnSaveLog.Enabled := memLog.Visible;
  gbxSkippedRecs.Enabled := Length(dmData.SkippedRecs) > 0;
  btnCreateScript.Enabled := dmData.Script <> '';
end;

//---------------------------------------------------------------------------

procedure TfrmUI.ShowQueriesCount;
begin
  lblQueriesCount.Caption := Format('%d/%d', [FSQLQueries.ItemIndex + 1,
    FSQLQueries.Count]);
end;

//---------------------------------------------------------------------------

procedure TfrmUI.ShowQueryResultGrid;
begin
  if grdQueryResult.Height < I_MIN_HEIGHT then
    grdQueryResult.Height := pnlQueryWorkspace.Height div 2;

  if not grdQueryResult.Visible then
  begin
    grdQueryResult.Visible := True;
    splQueryResult.Visible := True;
  end;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.splFileActionsMoved(Sender: TObject);
begin
  RecalcFileGridCols();
end;

//---------------------------------------------------------------------------

procedure TfrmUI.StepProcess(CurrentIteration: Integer);
begin
  pbFileProcess.StepIt();
  lblCurrentRecordNo.Caption := IntToStr(CurrentIteration);
end;

//---------------------------------------------------------------------------

procedure TfrmUI.tmrTimerTimer(Sender: TObject);
var
  Hr, Mn, Sc, Msc: Word;

begin
  DecodeTime(Now(), Hr, Mn, Sc, Msc);

  if (Hr = 12) and (Mn > 0) and (Mn < 45) then
  begin
    if not mnuBlowCoffee.Visible then
      mnuBlowCoffee.Visible := True;
  end
  else
  begin
    if mnuBlowCoffee.Visible then
      mnuBlowCoffee.Visible := False;
  end;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.Tray(n: Integer; Icon: TIcon);
var
  Nim: TNotifyIconData;
  CharArr: TCharArray;

begin
  CharArr := StringToArray(Application.Title);

  with Nim do
  begin
    cbSize := SizeOf(Nim);
    Wnd := Self.Handle;
    uID := 1;
    uFlags := NIF_ICON or NIF_MESSAGE or NIF_TIP;
    hicon := Icon.Handle;
    uCallbackMessage := WM_USER + 1;
    Move(CharArr, szTip, 64);
  end;

  case n of
    1: Shell_NotifyIcon(Nim_Add, @Nim);
    2: Shell_NotifyIcon(Nim_Delete, @Nim);
    3: Shell_NotifyIcon(Nim_Modify, @Nim);
  end;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.ValidationLog(Msg: AnsiString);
begin
  if not memLog.Visible then
    memLog.Visible := True;
  if not splLog.Visible then
    splLog.Visible := True;

  if memLog.Height < I_MIN_HEIGHT then
    memLog.Height := I_NORMAL_HEIGHT;

  memLog.Lines.Add(Msg);
end;

//---------------------------------------------------------------------------

procedure TfrmUI.WMDropFiles(var Msg: TWMDropFiles);
var
  DropH: hDrop;
  DroppedFileCount: Integer;
  FileNameLength: Integer;
  FileName: AnsiString;
  I: Integer;

begin
  inherited;

  DropH := Msg.Drop;
  try
    EnableFileControls(False);
    DroppedFileCount := DragQueryFile(DropH, I_FILE_INDEX, nil, 0);

    for I := 0 to Pred(DroppedFileCount) do
    begin
      FileNameLength := DragQueryFile(DropH, I, nil, 0);
      SetLength(FileName, FileNameLength);
      DragQueryFile(DropH, I, PChar(FileName), FileNameLength + 1);
      OpenFile(FileName);
    end;

  finally
    DragFinish(DropH);
  end;

  Msg.Result := 0;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.WriteParams;
begin
  with TRegistry.Create() do
    try
      RootKey := HKEY_CURRENT_USER;
      OpenKey(S_REG_KEY, True);
      WriteInteger(S_FORM_LEFT, frmUI.Left);
      WriteInteger(S_FORM_TOP, frmUI.Top);
      WriteInteger(S_FORM_HEIGHT, frmUI.Height);
      WriteInteger(S_FORM_WIDTH, frmUI.Width);
      WriteBool(S_COMPRESS_COLUMNS, FCompressColumns);
      WriteBool(S_MATH_ERRORS, chbMathErrors.Checked);
      WriteBool(S_DUPLICATES, chbDuplicates.Checked);
      WriteBool(S_RELATION_ERRORS, chbRelationErrors.Checked);
      WriteBool(S_DICT_REPLACES, chbDictReplaces.Checked);
      WriteBool(S_EMPTY_RECORDS, chbEmptyRecords.Checked);
      WriteBool(S_PREV_REPORT_SUM, chbPrevReportSum.Checked);

    finally
      CloseKey();
      Free();
    end;
end;

//---------------------------------------------------------------------------

procedure TfrmUI.WriteSettings;
begin
  with TIniFile.Create(ExtractFilePath(Application.ExeName) +
    S_SETTINGS_FILE_NAME) do
    try
      WriteString(S_INI_TEMPLATES, S_SELECT_TEMPLATE, SQLSelectTemplate);
      WriteString(S_INI_TEMPLATES, S_INSERT_TEMPLATE, SQLInsertTemplate);
      WriteString(S_INI_TEMPLATES, S_UPDATE_TEMPLATE, SQLUpdateTemplate);
      WriteString(S_INI_TEMPLATES, S_DELETE_TEMPLATE, SQLDeleteTemplate);
      WriteBool(S_INI_GUI, S_TRAY_ENABLED, TrayEnabled);
    finally
      Free();
    end;
end;

end.

