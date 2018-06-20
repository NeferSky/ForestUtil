unit Edit;

interface

uses
  Windows, Controls, Forms, StdCtrls, Classes, Dictionary, ForestTypes, Dialogs;

type
  TfrmEdit = class(TForm)
    edtWord: TEdit;
    lblTableValue: TLabel;
    btnOk: TButton;
    lblSynonim: TLabel;
    cmbSynonim: TComboBox;
    btnSkip: TButton;
    btnSkipAll: TButton;
    lblPrompt: TLabel;
    btnStop: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnSkipClick(Sender: TObject);
    procedure btnSkipAllClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure cmbSynonimChange(Sender: TObject);
    procedure cmbSynonimDropDown(Sender: TObject);
    procedure cmbSynonimKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FWaiting: Boolean;
    FDictionary: TDictionary;
    FCurrentIndex: Integer;
    FCurrentText: AnsiString;
    FInputText: AnsiString;
    procedure ReadSettings;
    procedure WriteSettings;
    function GetDictionary: TDictionary;
    procedure SetDictionary(Value: TDictionary);
  public
    { Public declarations }
    ShowResult: TShowResult;
    property Dictionary: TDictionary read GetDictionary write SetDictionary;
    property CurrentIndex: Integer read FCurrentIndex;
    property CurrentText: AnsiString read FCurrentText;
  end;

function ShowEdit(const OldWord, Prompt: AnsiString; const Dict: TDictionary): TShowResult;
function ShowEditEx(const OldWord, Prompt: AnsiString; const Dict: TDictionary; Modal: Boolean = False): TShowResult;

var
  frmEdit: TfrmEdit;

implementation

uses
  Registry, ForestConsts, StrUtils, SysUtils;

{$R *.dfm}

function ShowEdit(const OldWord, Prompt: AnsiString; const Dict: TDictionary): TShowResult;
begin
  frmEdit.FWaiting := True;
  frmEdit.edtWord.Text := OldWord;
  frmEdit.edtWord.ReadOnly := True;
  frmEdit.edtWord.TabStop := False;
  frmEdit.btnSkip.Enabled := True;
  frmEdit.btnSkipAll.Enabled := True;
  frmEdit.lblPrompt.Caption := Prompt;
  frmEdit.Dictionary := Dict;
  frmEdit.cmbSynonim.Text := '';
  frmEdit.FInputText := '';
  frmEdit.Show();

  while frmEdit.FWaiting do
    Application.ProcessMessages();

  Result := frmEdit.ShowResult;
end;

//---------------------------------------------------------------------------

function ShowEditEx(const OldWord, Prompt: AnsiString; const Dict: TDictionary; Modal: Boolean = False): TShowResult;
begin
  frmEdit.FWaiting := True;
  frmEdit.edtWord.Text := OldWord;
  frmEdit.edtWord.ReadOnly := False;
  frmEdit.edtWord.TabStop := True;
  frmEdit.btnSkip.Enabled := False;
  frmEdit.btnSkipAll.Enabled := False;
  frmEdit.lblPrompt.Caption := Prompt;
  frmEdit.Dictionary := Dict;
  frmEdit.cmbSynonim.Text := '';
  frmEdit.FInputText := '';

  if Modal then
    frmEdit.ShowModal()
  else
    frmEdit.Show();

  while frmEdit.FWaiting do
    Application.ProcessMessages();

  Result := frmEdit.ShowResult;
end;

//---------------------------------------------------------------------------
{ TfrmEdit }

procedure TfrmEdit.btnOkClick(Sender: TObject);
begin
  ShowResult := srReplace;
  Hide();
  FWaiting := False;
end;

//---------------------------------------------------------------------------

procedure TfrmEdit.btnSkipAllClick(Sender: TObject);
begin
  ShowResult := srForceSkip;
  Hide();
  FWaiting := False;
end;

//---------------------------------------------------------------------------

procedure TfrmEdit.btnSkipClick(Sender: TObject);
begin
  ShowResult := srSkip;
  Hide();
  FWaiting := False;
end;

//---------------------------------------------------------------------------

procedure TfrmEdit.btnStopClick(Sender: TObject);
begin
  ShowResult := srStop;
  Hide();
  FWaiting := False;
end;

//---------------------------------------------------------------------------

procedure TfrmEdit.cmbSynonimChange(Sender: TObject);
var
  I, Place: Integer;

begin
  for I := 0 to cmbSynonim.Items.Count - 1 do
  begin
    Place := AnsiPos(FInputText, cmbSynonim.Items[I]);

    if Place > 0 then
    begin
      cmbSynonim.Text := cmbSynonim.Items[I];
      cmbSynonim.SelStart := Place - 1;
      cmbSynonim.SelLength := Length(FInputText);
      Break;
    end;
  end;

  FCurrentIndex := FDictionary.GetRecordByListItem(cmbSynonim.Text).WordIndex;
  FCurrentText := FDictionary.GetRecordByListItem(cmbSynonim.Text).WordValue;
end;

//---------------------------------------------------------------------------

procedure TfrmEdit.cmbSynonimDropDown(Sender: TObject);
begin
  if cmbSynonim.Items.Count < 10 then
    cmbSynonim.DropDownCount := cmbSynonim.Items.Count;
  if cmbSynonim.Items.Count >= 10 then
    cmbSynonim.DropDownCount := 10;
end;

//---------------------------------------------------------------------------

procedure TfrmEdit.cmbSynonimKeyPress(Sender: TObject; var Key: Char);
begin
  if ((Ord(Key)) = 8) and (Length(FInputText) > 0) then
    Delete(FInputText, Length(FInputText), 1)
  else
    FInputText := FInputText + Key;

  FInputText := AnsiUpperCase(FInputText);
end;

//---------------------------------------------------------------------------

procedure TfrmEdit.FormCreate(Sender: TObject);
begin
  ReadSettings();
end;

//---------------------------------------------------------------------------

procedure TfrmEdit.FormDestroy(Sender: TObject);
begin
  WriteSettings();
end;
    
//---------------------------------------------------------------------------

function TfrmEdit.GetDictionary: TDictionary;
begin
  Result := FDictionary;
end;

//---------------------------------------------------------------------------

procedure TfrmEdit.ReadSettings;
begin
  with TRegistry.Create() do
    try
      RootKey := HKEY_CURRENT_USER;
      OpenKeyReadOnly(S_REG_KEY);

      if ValueExists(S_EDIT_LEFT) then
        frmEdit.Left := ReadInteger(S_EDIT_LEFT);
      if ValueExists(S_EDIT_TOP) then
        frmEdit.Top := ReadInteger(S_EDIT_TOP);
      if ValueExists(S_EDIT_HEIGHT) then
        frmEdit.Height := ReadInteger(S_EDIT_HEIGHT);
      if ValueExists(S_EDIT_WIDTH) then
        frmEdit.Width := ReadInteger(S_EDIT_WIDTH);

    finally
      CloseKey();
      Free();
    end;
end;
  
//---------------------------------------------------------------------------

procedure TfrmEdit.SetDictionary(Value: TDictionary);
begin
  if FDictionary <> Value then
  begin
    FDictionary := Value;
    cmbSynonim.Clear();
    cmbSynonim.Items.Assign(Value.DictionaryList);
    cmbSynonim.Sorted := True;
  end;
end;

//---------------------------------------------------------------------------

procedure TfrmEdit.WriteSettings;
begin
  with TRegistry.Create() do
    try
      RootKey := HKEY_CURRENT_USER;
      OpenKey(S_REG_KEY, True);

      WriteInteger(S_EDIT_LEFT, frmEdit.Left);
      WriteInteger(S_EDIT_TOP, frmEdit.Top);
      WriteInteger(S_EDIT_HEIGHT, frmEdit.Height);
      WriteInteger(S_EDIT_WIDTH, frmEdit.Width);
    finally
      CloseKey();
      Free();
    end;
end;

end.

