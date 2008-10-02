unit Fontdlg;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ColorGrd, Htmlview, Spin;

type
  TFontForm = class(TForm)
    FontListBox: TListBox;
    FontColorGrid: TColorGrid;
    HotSpotColorGrid: TColorGrid;
    BackListBox: TListBox;
    OKButton: TButton;
    Cancel: TButton;
    ResetButton: TButton;
    FontSizeEdit: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    FontViewer: THTMLViewer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure HotSpotColorGridChange(Sender: TObject);
    procedure FontColorGridChange(Sender: TObject);
    procedure ListBoxClicks(Sender: TObject);
  private
    { Private declarations }
    FFontColor: TColor;
    FHotSpotColor: TColor;
    FFontSize: integer;
    InitialFontName: string;
    InitialFontSize: integer;
    InitialFontColor: TColor;
    InitialHotSpotColor: TColor;
    InitialBackground: TColor;
    procedure AddItem(const Value: string);
    function GetFontName: TFontName;
    procedure SetFontName(Value: TFontName);
    function GetBackground: TColor;
    procedure SetBackground(Value: TColor);
    procedure SetFontColor(Value: TColor);
    procedure SetHotSpotColor(Value: TColor);
    procedure SetFontSize(Value: integer);
    procedure LoadAgain;
  public
    { Public declarations }
    property FontName: TFontName read GetFontName write SetFontName;
    property Background: TColor read GetBackground write SetBackground;
    property FontColor: TColor read FFontColor write SetFontColor;
    property FontSize: integer read FFontSize write SetFontSize;
    property HotSpotColor: TColor read FHotSpotColor write SetHotSpotColor;
  end;

var
  FontForm: TFontForm;

implementation

{$R *.DFM}
const
  ViewText: string =
     '<center><h1>Heading</h1></center>'+
     '<ul>Some normal text.'+
     '<li><a href=NoWhere>HotSpot Item</a>'+
     '<li><b>Bold Text</b>'+
     '<li><i>Italicized Text</i>'+
     '<li><code>Code Text</code>'+
     '</ul> '+
     '<hr>';

procedure TFontForm.LoadAgain;
begin
FontViewer.LoadFromBuffer(@ViewText[1], Length(ViewText), '');
end;

procedure TFontForm.FormCreate(Sender: TObject);
begin
FontListBox.Items := Screen.Fonts;
GetColorValues(AddItem);
LoadAgain;
end;

procedure TFontForm.AddItem(const Value: string);
var
  Color: TColor;
begin
Color := StringToColor(Value);
if (Color >= 0) or (Color = -16) or (Color = -6) or (Color = -2) then
  BackListBox.Items.Add(Value);
end;

function TFontForm.GetFontName: TFontName;
begin
try
  Result := FontListBox.Items[FontListBox.ItemIndex];
except
  Result := 'System';   {in case nothing hilited}
  end;
end;

procedure TFontForm.SetFontName(Value: TFontName);
var
  I: integer;
begin
I := FontListBox.Items.IndexOf(Value);
if I < 0 then
  I := FontListBox.Items.IndexOf('System');
FontListBox.ItemIndex := I;
FontViewer.DefFontName := Value;
LoadAgain;
end;

function TFontForm.GetBackground: TColor;
begin
try
  Result := StringToColor(BackListBox.Items[BackListBox.ItemIndex]);
except
  Result := clBtnFace;
  end;
end;

procedure TFontForm.SetBackground(Value: TColor);
var
  I: integer;
  S: string[80];
begin
S := ColorToString(Value);
I := BackListBox.Items.IndexOf(S);
if I < 0 then
  begin
  BackListBox.Items.Add(S);
  I := BackListBox.Items.IndexOf(S);
  end;
BackListBox.ItemIndex := I;
FontViewer.DefBackground := Value;
end;

procedure TFontForm.SetFontSize(Value: integer);
begin
FontViewer.DefFontSize := Value;
FFontSize := Value;
FontSizeEdit.Value := Value;
LoadAgain;
end;

procedure TFontForm.SetFontColor(Value: TColor);
begin
FontViewer.DefFontColor := Value;
FFontColor := Value;
FontColorGrid.ForegroundEnabled := False;
LoadAgain;
end;

procedure TFontForm.SetHotSpotColor(Value: TColor);
begin
FontViewer.DefHotSpotColor := Value;
FHotSpotColor := Value;
HotSpotColorGrid.ForegroundEnabled := False;
LoadAgain;
end;

procedure TFontForm.FormShow(Sender: TObject);
begin
InitialFontName := GetFontName;
InitialFontColor := FFontColor;
InitialHotSpotColor := FHotSpotColor;
InitialBackground := GetBackground;
InitialFontSize := FFontSize;
end;

procedure TFontForm.ResetButtonClick(Sender: TObject);
begin
FontName := InitialFontName;
FontSize := InitialFontSize;
FontColor := InitialFontColor;
HotSpotColor := InitialHotSpotColor;
Background := InitialBackground;
end;

procedure TFontForm.HotSpotColorGridChange(Sender: TObject);
begin
HotSpotColor := HotSpotColorGrid.ForegroundColor;
end;

procedure TFontForm.FontColorGridChange(Sender: TObject);
begin
FontColor := FontColorGrid.ForegroundColor;
end;

procedure TFontForm.ListBoxClicks(Sender: TObject);
begin
if Sender = FontListBox then
  FontName := FontListBox.Items[FontListBox.ItemIndex]
else if Sender = BackListBox then
  Background := StringToColor(BackListBox.Items[BackListBox.ItemIndex])
else if Sender = FontSizeEdit then
  FontSize := FontSizeEdit.Value;
end;

end.
