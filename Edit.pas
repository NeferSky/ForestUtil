unit Edit;

interface

uses
  Windows, Controls, Forms, StdCtrls, Classes;

type
  TfrmEdit = class(TForm)
    edtWord: TEdit;
    lblTableValue: TLabel;
    btnOk: TButton;
    lblSynonim: TLabel;
    cmbSynonim: TComboBox;
    btnSkip: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure ReadSettings;
    procedure WriteSettings;
    function GetValuesList: TStringList;
    procedure SetValuesList(Value: TStringList);
  public
    { Public declarations }
    property ValuesList: TStringList read GetValuesList write SetValuesList;
  end;

var
  frmEdit: TfrmEdit;

implementation

uses
  Registry, ForestConsts;

{$R *.dfm}

//---------------------------------------------------------------------------
{ TfrmEdit }

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

function TfrmEdit.GetValuesList: TStringList;
begin
  Result.Assign(cmbSynonim.Items);
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

procedure TfrmEdit.SetValuesList(Value: TStringList);
begin
  cmbSynonim.Clear();
  cmbSynonim.Items.Assign(Value);
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

