unit UIButton;

interface

uses
  Windows, ExtCtrls, Classes, StdCtrls, Controls, Graphics, Dialogs, Math;

type
  TUIButton = class (TPanel)
  private
    FText: string;
    procedure MouseEnter (Sender: TObject);
    procedure MouseLeave (Sender: TObject);
    procedure SetText(const Value: string);
  published
    constructor Create (AOwner : TComponent); override;
    procedure Paint; override;
    property Text : string read FText write SetText;
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
    Canvas.TextOut(50, floor((height - (Canvas.Font.Size * 1.2))/2), Text);
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
    Canvas.TextOut(50, floor((height - (Canvas.Font.Size * 1.2))/2), Text);
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
    Canvas.TextOut(50, floor((height - (Canvas.Font.Size * 1.2))/2), Text);
  finally
    Bitmap.Free;
  end;
end;

procedure TUIButton.SetText(const Value: string);
begin
  FText := Value;
end;

end.