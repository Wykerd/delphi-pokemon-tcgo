unit UIButton;

interface

uses
  Windows, ExtCtrls, Classes, StdCtrls, Controls, Graphics, Dialogs, Math;

type
  TUIButton = class (TPanel)
  private
    FText: string;
    FFontSize: integer;
    procedure MouseEnter (Sender: TObject);
    procedure MouseLeave (Sender: TObject);
    procedure SetText(const Value: string);
    procedure SetFontSize(const Value: integer);
  published
    constructor Create (AOwner : TComponent); override;
    procedure Paint; override;
    property Text : string read FText write SetText;
    property FontSize : integer read FFontSize write SetFontSize;
  end;

implementation

{ TUIButton }

constructor TUIButton.Create(AOwner: TComponent);
var
  Bitmap : TBitmap;
begin
  inherited;

  BevelEdges := [];
  BevelOuter := bvNone;

  OnMouseEnter := MouseEnter;
  OnMouseLeave := MouseLeave;

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Size := 15;
  Canvas.Font.Name := 'Lucida Console';
end;

procedure TUIButton.MouseEnter(Sender: TObject);
var
  Bitmap : TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(HInstance, 'BlankResource');
    Canvas.StretchDraw(Self.ClientRect, Bitmap);
    Canvas.Font.Color := rgb(222, 77, 64);
    Canvas.TextOut(floor(width / 10), floor((height - (Canvas.Font.Size * 1.2))/2), Text);
  finally
    Bitmap.Free;
  end;
end;

procedure TUIButton.MouseLeave(Sender: TObject);
var
  Bitmap : TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(HInstance, 'BlankResource');
    Canvas.StretchDraw(Self.ClientRect, Bitmap);
    Canvas.Font.Color := clBlack;
    Canvas.TextOut(floor(width / 10), floor((height - (Canvas.Font.Size * 1.2))/2), Text);
  finally
    Bitmap.Free;
  end;
end;

procedure TUIButton.Paint;
var
  Bitmap : TBitmap;
begin
  inherited;

  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(HInstance, 'BlankResource');
    Canvas.StretchDraw(Self.ClientRect, Bitmap);
    Canvas.Font.Color := clBlack;
    Canvas.TextOut(floor(width / 10), floor((height - (Canvas.Font.Size * 1.2))/2), Text);
  finally
    Bitmap.Free;
  end;
end;

procedure TUIButton.SetFontSize(const Value: integer);
begin
  FFontSize := Value;
  Canvas.Font.Size := Value;
end;

procedure TUIButton.SetText(const Value: string);
begin
  FText := Value;
end;

end.