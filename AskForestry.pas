unit AskForestry;

interface

uses
  SysUtils, Controls, Forms, StdCtrls, Classes, ComCtrls, ForestTypes;

type
  TfrmAskForestry = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    gbxForestry: TGroupBox;
    lblForestry: TLabel;
    lblRegion: TLabel;
    cmbForestry: TComboBox;
    cmbRegion: TComboBox;
    gbxReportPeriod: TGroupBox;
    cmbQuarter: TComboBox;
    lblMonth: TLabel;
    lblYear: TLabel;
    edtYear: TEdit;
    udYear: TUpDown;
    procedure cmbRegionSelect(Sender: TObject);
  private
    { Private declarations }
    FRegionID: Integer;
    FForestryID: Integer;
    FReportQuarter: Integer;
    FReportYear: Integer;
  public
    { Public declarations }
    property RegionID: Integer read FRegionID write FRegionID;
    property ForestryID: Integer read FForestryID write FForestryID;
    property ReportQuarter: Integer read FReportQuarter write FReportQuarter;
    property ReportYear: Integer read FReportYear write FReportYear;
  end;

function AskFileForestry: Boolean;

var
  frmAskForestry: TfrmAskForestry;

implementation

uses
  Data, ForestConsts, Dictionary, Dialogs;

{$R *.dfm}

function AskFileForestry: Boolean;
var
  AYear, AMonth, ADay: Word;

begin
  frmAskForestry.cmbRegionSelect(Application);

  DecodeDate(Date(), AYear, AMonth, ADay);
  case AMonth of
    1..3:
      begin
        frmAskForestry.udYear.Position := AYear - 1;
        frmAskForestry.cmbQuarter.ItemIndex := 3;
      end;
    else
      begin
        frmAskForestry.udYear.Position := AYear;
        frmAskForestry.cmbQuarter.ItemIndex := ((AMonth - 1) div 3) - 1;
      end;
  end;

  frmAskForestry.ShowModal();

  frmAskForestry.RegionID :=
    dmData.GetIntField(Format(S_DB_GET_REGION_ID_BY_FORESTRY,
    [frmAskForestry.cmbForestry.Text]));
  frmAskForestry.ForestryID := dmData.GetIntField(Format(S_DB_GET_FORESTRY_ID,
    [frmAskForestry.cmbForestry.Text]));

  frmAskForestry.ReportQuarter := frmAskForestry.cmbQuarter.ItemIndex + 1;
  frmAskForestry.ReportYear := frmAskForestry.udYear.Position;

  Result := frmAskForestry.ModalResult = mrOk;
end;

//---------------------------------------------------------------------------

procedure TfrmAskForestry.cmbRegionSelect(Sender: TObject);
begin
  case cmbRegion.ItemIndex of
    0: cmbForestry.Items.Assign(dmData.GetForestryByRegion(22));
    1: cmbForestry.Items.Assign(dmData.GetForestryByRegion(23));
  end;

  cmbForestry.ItemIndex := 0;
end;

end.

