unit AskForestry;

interface

uses
  SysUtils, Controls, Forms, StdCtrls, Classes;

type
  TfrmAskForestry = class(TForm)
    lblForestry: TLabel;
    cmbForestry: TComboBox;
    btnOk: TButton;
    btnCancel: TButton;
    lblRegion: TLabel;
    cmbRegion: TComboBox;
    procedure cmbRegionSelect(Sender: TObject);
  private
    { Private declarations }
    FRegionID: Integer;
    FForestryID: Integer;
//    FLocalForestryID: Integer;
  public
    { Public declarations }
    property RegionID: Integer read FRegionID write FRegionID;
    property ForestryID: Integer read FForestryID write FForestryID;
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

  frmAskForestry.cmbRegionSelect(Application);

  frmAskForestry.ShowModal();

  frmAskForestry.RegionID := dmData.GetIntField(Format(
    S_DB_GET_REGION_ID_BY_FORESTRY, [frmAskForestry.cmbForestry.Text]));
  frmAskForestry.ForestryID := dmData.GetIntField(Format(
    S_DB_GET_FORESTRY_ID, [frmAskForestry.cmbForestry.Text]));

  Result := frmAskForestry.ModalResult = mrOk;
end;

//---------------------------------------------------------------------------

procedure TfrmAskForestry.cmbRegionSelect(Sender: TObject);
begin
  case cmbRegion.ItemIndex of
  0: cmbForestry.Items.Assign(dmData.GetForestry(22));
  1: cmbForestry.Items.Assign(dmData.GetForestry(23));
  end;

  cmbForestry.ItemIndex := 0;
end;

end.

