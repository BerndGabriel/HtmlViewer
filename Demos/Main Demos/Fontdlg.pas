unit Fontdlg;

{$include ..\..\source\htmlcons.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Spin, Buttons,
  StyleUn, HTMLUn2, Htmlview;

type
  TFontForm = class(TForm)
    BackgroundColorButton: TSpeedButton;
    BackgroundColorComboBox: TComboBox;
    BackgroundColorLabel: TLabel;
    Cancel: TButton;
    ColorDialog: TColorDialog;
    FontColorButton: TSpeedButton;
    FontColorComboBox: TComboBox;
    FontColorLabel: TLabel;
    FontDialog: TFontDialog;
    FontNameComboBox: TComboBox;
    FontNameLabel: TLabel;
    FontSizeEdit: TSpinEdit;
    FontSizeLabel: TLabel;
    FontViewer: THTMLViewer;
    LinkColorButton: TSpeedButton;
    LinkColorComboBox: TComboBox;
    LinkColorLabel: TLabel;
    OKButton: TButton;
    ResetButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure FontSizeEditClick(Sender: TObject);
    procedure AnythingChanged(Sender: TObject);
    procedure FontColorButtonClick(Sender: TObject);
    procedure LinkColorButtonClick(Sender: TObject);
    procedure BackgroundColorButtonClick(Sender: TObject);
    procedure FontNameButtonClick(Sender: TObject);
  private
    Colors: TStringList;
    CustomColorIndex: Integer;
    InitialFontName: string;
    InitialFontSize: integer;
    InitialFontColor: TColor;
    InitialHotSpotColor: TColor;
    InitialBackground: TColor;
    function GetFontName: TFontName;
    procedure SetFontName(Value: TFontName);
    function GetBackground: TColor;
    procedure SetBackground(Value: TColor);
    procedure SetFontColor(Value: TColor);
    procedure SetHotSpotColor(Value: TColor);
    procedure SetFontSize(Value: integer);
    procedure LoadAgain;
    function GetFontColor: TColor;
    function GetHotSpotColor: TColor;
    function GetFontSize: integer;
  public
    { Public declarations }
    property FontName: TFontName read GetFontName write SetFontName;
    property Background: TColor read GetBackground write SetBackground;
    property FontColor: TColor read GetFontColor write SetFontColor;
    property FontSize: integer read GetFontSize write SetFontSize;
    property HotSpotColor: TColor read GetHotSpotColor write SetHotSpotColor;
  end;

implementation

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

const
  CustomColor = 'Custom color';
  ViewText =
   '<center><h1>Heading</h1></center>'+
   '<ul>Some normal text.'+
   '<li><a href=NoWhere>HotSpot Item</a>'+
   '<li><b>Bold Text</b>'+
   '<li><i>Italicized Text</i>'+
   '<li><code>Code Text</code>'+
   '</ul> '+
   '<hr>';

function ColorOfIndex(Colors: TStrings; Index: Integer; DefaultColor: TColor): TColor;
begin
  try
    Result := TColor(Colors.Objects[Index]);
  except
    Result := DefaultColor;
  end;
end;

function IndexOfColor(Colors: TStrings; Color: TColor; CustomColorIndex: Integer): Integer;
begin
  Result := Colors.IndexOfObject(TObject(Color));
  if Result < 0 then
  begin
    Result := CustomColorIndex;
    Colors.Objects[Result] := TObject(Color);
  end;
end;

procedure TFontForm.AnythingChanged(Sender: TObject);
begin
  FontViewer.DefBackground := Background;
  FontViewer.DefFontColor := FontColor;
  FontViewer.DefFontName := FontName;
  FontViewer.DefFontSize := FontSize;
  FontViewer.DefHotSpotColor := HotSpotColor;
  LoadAgain;
end;

procedure TFontForm.BackgroundColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := Background;
  if ColorDialog.Execute then
    Background := ColorDialog.Color;
end;

procedure TFontForm.FontColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := FontColor;
  if ColorDialog.Execute then
    FontColor := ColorDialog.Color;
end;

procedure TFontForm.FontNameButtonClick(Sender: TObject);
begin
  FontDialog.Font.Name := FontName;
  FontDialog.Font.Size := FontSize;
  if FontDialog.Execute then
  begin
    FontName := FontDialog.Font.Name;
    FontSize := FontDialog.Font.Size;
  end;
end;

procedure TFontForm.LinkColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := HotSpotColor;
  if ColorDialog.Execute then
    HotSpotColor := ColorDialog.Color;
end;

procedure TFontForm.FontSizeEditClick(Sender: TObject);
begin
  FontSize := FontSizeEdit.Value;
end;

procedure TFontForm.FormCreate(Sender: TObject);
begin
  FontNameComboBox.Items := Screen.Fonts;
  Colors := TStringList.Create;
  Colors.Assign(StyleUn.SortedColors);
  Colors.Sorted := True;
  CustomColorIndex := Colors.Add(CustomColor);
  FontColorComboBox.Items := Colors;
  LinkColorComboBox.Items := Colors;
  BackgroundColorComboBox.Items := Colors;
  LoadAgain;
end;

procedure TFontForm.FormShow(Sender: TObject);
begin
  InitialFontName := FontName;
  InitialFontColor := FontColor;
  InitialHotSpotColor := HotSpotColor;
  InitialBackground := Background;
  InitialFontSize := FontSize;
end;

function TFontForm.GetBackground: TColor;
begin
  with BackgroundColorComboBox do
    Result := ColorOfIndex(Items, ItemIndex, InitialBackground);
end;

function TFontForm.GetFontColor: TColor;
begin
  with FontColorComboBox do
    Result := ColorOfIndex(Items, ItemIndex, InitialFontColor);
end;

function TFontForm.GetFontName: TFontName;
begin
  try
    Result := FontNameComboBox.Text;
  except
    Result := InitialFontName;   {in case nothing hilited}
  end;
end;

function TFontForm.GetFontSize: integer;
begin
  try
    Result := FontSizeEdit.Value;
  except
    Result := InitialFontSize;
  end;
end;

function TFontForm.GetHotSpotColor: TColor;
begin
  with LinkColorComboBox do
    Result := ColorOfIndex(Items, ItemIndex, InitialHotSpotColor);
end;

procedure TFontForm.LoadAgain;
begin
  FontViewer.LoadFromString(ViewText, '');
end;

procedure TFontForm.ResetButtonClick(Sender: TObject);
begin
  FontName := InitialFontName;
  FontSize := InitialFontSize;
  FontColor := InitialFontColor;
  HotSpotColor := InitialHotSpotColor;
  Background := InitialBackground;
end;

procedure TFontForm.SetBackground(Value: TColor);
begin
  with BackgroundColorComboBox do
    ItemIndex := IndexOfColor(Items, Value, CustomColorIndex);
  FontViewer.DefBackground := Value;
end;

procedure TFontForm.SetFontColor(Value: TColor);
begin
  with FontColorComboBox do
    ItemIndex := IndexOfColor(Items, Value, CustomColorIndex);
  FontViewer.DefFontColor := Value;
  LoadAgain;
end;

procedure TFontForm.SetFontName(Value: TFontName);
var
  I: integer;
begin
  I := FontNameComboBox.Items.IndexOf(Value);
  if I < 0 then
    I := FontNameComboBox.Items.IndexOf('System');
  FontNameComboBox.ItemIndex := I;
  FontViewer.DefFontName := Value;
  LoadAgain;
end;

procedure TFontForm.SetFontSize(Value: integer);
begin
  FontSizeEdit.Value := Value;
  FontViewer.DefFontSize := Value;
  LoadAgain;
end;

procedure TFontForm.SetHotSpotColor(Value: TColor);
begin
  with LinkColorComboBox do
    ItemIndex := IndexOfColor(Items, Value, CustomColorIndex);
  FontViewer.DefHotSpotColor := Value;
  LoadAgain;
end;

end.
