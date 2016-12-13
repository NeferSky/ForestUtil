unit Edit;

interface

uses
  Windows, Controls, Forms, StdCtrls, Classes, ForestTypes;

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
  private
    { Private declarations }
    FWaiting: Boolean;
    procedure ReadSettings;
    procedure WriteSettings;
  public
    { Public declarations }
    ShowResult: TShowResult;
  end;

  function ShowEdit(const OldWord, Prompt: AnsiString;
    const Dict: TStringList): TShowResult;

var
  frmEdit: TfrmEdit;

implementation

uses
  Registry, ForestConsts;

{$R *.dfm}

function ShowEdit(const OldWord, Prompt: AnsiString;
  const Dict: TStringList): TShowResult;
begin
  frmEdit.FWaiting := True;
  frmEdit.edtWord.Text := OldWord;
  frmEdit.lblPrompt.Caption := Prompt;
  frmEdit.cmbSynonim.Clear();
  frmEdit.cmbSynonim.Items.Assign(Dict);
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

