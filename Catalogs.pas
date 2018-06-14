unit Catalogs;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, ForestTypes,
  ComCtrls, Grids, Dialogs;

type
  TfrmCatalogs = class(TForm)
    lblCatalog: TLabel;
    lblValues: TLabel;
    cmbCatalogs: TComboBox;
    btnOk: TButton;
    btnApply: TButton;
    btnCancel: TButton;
    lvValues: TListView;
    btnClear: TButton;
    btnEdit: TButton;
    btnAdd: TButton;
    btnDelete: TButton;
    StatusBar1: TStatusBar;
    procedure Apply;
    procedure btnApplyClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure cmbCatalogsSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure lvValuesDblClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lvValuesClick(Sender: TObject);
    procedure lvValuesColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvValuesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
  private
    { Private declarations }
    FWordsArr: TCatalogArr;
    FCheckedCount: Integer;
    FSortedColumn: Integer;
    FDescending: Boolean;
    procedure EditItem(ItemIndex: Integer; MultiValue: Boolean = False); 
    procedure ResetControls;
    function GetClickedItemIndex: Integer;
  public
    { Public declarations }
  end;

var
  frmCatalogs: TfrmCatalogs;

implementation

uses
  Data, ForestConsts, Edit;

{$R *.dfm}

//---------------------------------------------------------------------------
{ TfrmCatalogs }

procedure TfrmCatalogs.Apply;
var
  I, NewLength: Integer;

begin
  SetLength(FWordsArr, 0);

  for I := 0 to lvValues.Items.Count - 1 do
  begin
    NewLength := Length(FWordsArr);
    SetLength(FWordsArr, NewLength + 1);

    FWordsArr[I].OldWord := lvValues.Items.Item[I].Caption;
    FWordsArr[I].NewWord := lvValues.Items.Item[I].SubItems[0];
    FWordsArr[I].NewIndex := StrToInt(lvValues.Items.Item[I].SubItems[1]);
  end;

  dmData.Validator.GetDictionary(cmbCatalogs.Text).CatalogArr := FWordsArr;
  cmbCatalogsSelect(Application);
end;
    
//---------------------------------------------------------------------------

procedure TfrmCatalogs.btnAddClick(Sender: TObject);
var
  Item: TListItem;

begin
  Item := lvValues.Items.Add;
  Item.Caption := '';
  Item.SubItems.Add('');
  Item.SubItems.Add('');

  EditItem(lvValues.Items.IndexOf(Item));
  ResetControls();
end;

//---------------------------------------------------------------------------

procedure TfrmCatalogs.btnApplyClick(Sender: TObject);
begin
  Apply();
  ResetControls();
end;
   
//---------------------------------------------------------------------------
     
procedure TfrmCatalogs.btnClearClick(Sender: TObject);
begin
  lvValues.Clear();
  ResetControls();
end;
  
//---------------------------------------------------------------------------

procedure TfrmCatalogs.btnDeleteClick(Sender: TObject);
var
  I: Integer;

begin
  for I := lvValues.Items.Count - 1 downto 0 do
    if lvValues.Items.Item[I].Checked then
      lvValues.Items.Delete(I);

  ResetControls();
end;

//---------------------------------------------------------------------------

procedure TfrmCatalogs.btnEditClick(Sender: TObject);
var
  I, EditedID: Integer;

begin
  EditedID := -1;

  for I := 0 to lvValues.Items.Count - 1 do
    if lvValues.Items.Item[I].Checked then
    begin
      EditedID := I;
      EditItem(EditedID, True);
      Break;
    end;

  if EditedID = -1 then
    Exit;

  for I := 0 to lvValues.Items.Count - 1 do
  begin
    if I = EditedID then // dot't touch edited item
      Continue;

    if lvValues.Items.Item[I].Checked then
    begin
      lvValues.Items.Item[I].SubItems[0] := lvValues.Items.Item[EditedID].SubItems[0];
      lvValues.Items.Item[I].SubItems[1] := lvValues.Items.Item[EditedID].SubItems[1];
    end;
  end;

  ResetControls();
end;

//---------------------------------------------------------------------------

procedure TfrmCatalogs.btnOkClick(Sender: TObject);
begin
  Apply();
  ResetControls();
end;

//---------------------------------------------------------------------------

procedure TfrmCatalogs.cmbCatalogsSelect(Sender: TObject);
var
  I: Integer;
  Item: TListItem;

begin
  lvValues.Clear();
  FWordsArr := dmData.Validator.GetDictionary(cmbCatalogs.Text).CatalogArr;

  for I := 0 to Length(FWordsArr) - 1 do
  begin
    Item := lvValues.Items.Add;
    Item.Caption := FWordsArr[I].OldWord;
    Item.SubItems.Add(FWordsArr[I].NewWord);
    Item.SubItems.Add(IntToStr(FWordsArr[I].NewIndex));
  end;

  ResetControls();
end;

//---------------------------------------------------------------------------

procedure TfrmCatalogs.EditItem(ItemIndex: Integer; MultiValue: Boolean = False);
var
  Prompt: AnsiString;

begin
  if MultiValue then
    Prompt := 'Несколько значений'
  else
    Prompt := lvValues.Items.Item[ItemIndex].Caption;

  ShowEditEx(Prompt, 'Выберите новое значение', dmData.Validator.GetDictionary(cmbCatalogs.Text), True);

  lvValues.Items.Item[ItemIndex].Caption := frmEdit.edtWord.Text;
  lvValues.Items.Item[ItemIndex].SubItems[0] := frmEdit.CurrentText;
  lvValues.Items.Item[ItemIndex].SubItems[1] := IntToStr(frmEdit.CurrentIndex);
end;

//---------------------------------------------------------------------------

procedure TfrmCatalogs.FormCreate(Sender: TObject);
begin
  cmbCatalogs.Clear();
  // Yes, this! Because i can modify caption later
  cmbCatalogs.Items.Add(dmData.Validator.GetDictionary(S_DICT_FORESTRIES_NAME).Caption);
  cmbCatalogs.Items.Add(dmData.Validator.GetDictionary(S_DICT_LOCAL_FORESTRIES_NAME).Caption);
  cmbCatalogs.Items.Add(dmData.Validator.GetDictionary(S_DICT_LANDUSE_NAME).Caption);
  cmbCatalogs.Items.Add(dmData.Validator.GetDictionary(S_DICT_PROTECT_CATEGORY_NAME).Caption);
  cmbCatalogs.Items.Add(dmData.Validator.GetDictionary(S_DICT_SPECIES_NAME).Caption);
  cmbCatalogs.Items.Add(dmData.Validator.GetDictionary(S_DICT_DAMAGE_NAME).Caption);
  cmbCatalogs.Items.Add(dmData.Validator.GetDictionary(S_DICT_PEST_NAME).Caption);
  cmbCatalogs.ItemIndex := 0;
  cmbCatalogsSelect(Sender);

  ResetControls();
end;

//---------------------------------------------------------------------------

function TfrmCatalogs.GetClickedItemIndex: Integer;
var
  CursorPos: TPoint;
  Item: TListItem;

begin
  GetCursorPos(CursorPos);
  CursorPos := ScreenToClient(CursorPos);
  CursorPos.X := CursorPos.X - lvValues.Left - 2;
  CursorPos.Y := CursorPos.Y - lvValues.Top;

  try
    Item := lvValues.GetItemAt(CursorPos.X, CursorPos.Y);
    Result := lvValues.Items.IndexOf(Item);
  except
    Result := -1;
  end;
end;

//---------------------------------------------------------------------------

procedure TfrmCatalogs.lvValuesClick(Sender: TObject);
begin
  ResetControls();
end;
   
//---------------------------------------------------------------------------

procedure TfrmCatalogs.lvValuesColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  TListView(Sender).SortType := stNone;

  if Column.Index <> FSortedColumn then
  begin
    FSortedColumn := Column.Index;
    FDescending := False;
  end
  else
    FDescending := not FDescending;

  TListView(Sender).SortType := stText;
end;
    
//---------------------------------------------------------------------------

procedure TfrmCatalogs.lvValuesCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  if FSortedColumn = 0 then
    Compare := CompareText(Item1.Caption, Item2.Caption)
  else
    if FSortedColumn <> 0 then
      Compare := CompareText(Item1.SubItems[FSortedColumn - 1], Item2.SubItems[FSortedColumn - 1]);

  if FDescending then Compare := -Compare;
end;

//---------------------------------------------------------------------------

procedure TfrmCatalogs.lvValuesDblClick(Sender: TObject);
var
  ItemIndex: Integer;

begin
  ItemIndex := GetClickedItemIndex();

  if ItemIndex = -1 then
    Exit;

  EditItem(ItemIndex);
  ResetControls();
end;

//---------------------------------------------------------------------------

procedure TfrmCatalogs.ResetControls;
var
  I: Integer;

begin
  FCheckedCount := 0;
  for I := 0 to lvValues.Items.Count - 1 do
    if lvValues.Items.Item[I].Checked then
      Inc(FCheckedCount);

  btnDelete.Enabled := FCheckedCount > 0;
  btnEdit.Enabled := FCheckedCount > 0;
end;

end.

