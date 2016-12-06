unit AskForestry;

interface

uses
  SysUtils, Controls, Forms, StdCtrls, Classes;

type
  TfrmAskForestry = class(TForm)
    lblForestry: TLabel;
    lblLocalForestry: TLabel;
    cmbForestry: TComboBox;
    cmbLocalForestry: TComboBox;
    btnOk: TButton;
    procedure cmbForestrySelect(Sender: TObject);
    procedure cmbLocalForestrySelect(Sender: TObject);
  private
    { Private declarations }
    FRegionID: Integer;
    FForestryID: Integer;
    FLocalForestryID: Integer;
    function IsForestriesSelected: Boolean;
  public
    { Public declarations }
    property RegionID: Integer read FRegionID write FRegionID;
    property ForestryID: Integer read FForestryID write FForestryID;
    property LocalForestryID: Integer read FLocalForestryID write FLocalForestryID;
  end;

  function AskFileForestry: Boolean;

var
  frmAskForestry: TfrmAskForestry;

implementation

uses
  Dicts, Data, ForestConsts;

{$R *.dfm}

function AskFileForestry: Boolean;
begin
  frmAskForestry.cmbForestry.Items.Assign(dmData.GetValidList(
    frmDicts.GetFileForDict(S_DICTIONARY_FORESTRIES_NAME)));
  frmAskForestry.cmbLocalForestry.Items.Assign(dmData.GetValidList(
    frmDicts.GetFileForDict(S_DICTIONARY_LOCAL_FORESTRIES_NAME)));

  frmAskForestry.cmbForestry.ItemIndex := 0;
  frmAskForestry.cmbLocalForestry.Text := '';
  frmAskForestry.btnOk.Enabled := frmAskForestry.IsForestriesSelected();
  frmAskForestry.cmbForestrySelect(Application);

  frmAskForestry.ShowModal();

  frmAskForestry.RegionID := dmData.GetIntField(Format(
    S_DB_GET_REGION_ID_BY_FORESTRY, [frmAskForestry.cmbForestry.Text]));
  frmAskForestry.ForestryID := dmData.GetIntField(Format(
    S_DB_GET_FORESTRY_ID, [frmAskForestry.cmbForestry.Text]));
  frmAskForestry.LocalForestryID := dmData.GetIntField(Format(
    S_DB_GET_LOCAL_FORESTRY_ID, [frmAskForestry.cmbLocalForestry.Text]));

  Result := True;
end;

//---------------------------------------------------------------------------

procedure TfrmAskForestry.cmbForestrySelect(Sender: TObject);
begin
  cmbLocalForestry.Items.Assign(dmData.GetLocalForestry(cmbForestry.Text));
  cmbLocalForestry.ItemIndex := 0;

  btnOk.Enabled := IsForestriesSelected();
end;
       
//---------------------------------------------------------------------------

procedure TfrmAskForestry.cmbLocalForestrySelect(Sender: TObject);
begin
  btnOk.Enabled := IsForestriesSelected();
end;
      
//---------------------------------------------------------------------------

function TfrmAskForestry.IsForestriesSelected: Boolean;
begin
  Result := (cmbForestry.Text <> '') and (cmbLocalForestry.Text <> '');
end;

end.

