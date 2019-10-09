unit UIButton;

interface

uses
  Windows, ExtCtrls, Classes, StdCtrls, Controls, Graphics, Dialogs, Math, Variants;

type
  TUIButton = class (TPanel)
  private
    FText: string;
    FFontSize: integer;
    FTextOffset: real;
    procedure SetText(const Value: string);
    procedure SetFontSize(const Value: integer);
    procedure HandleResize (Sender : TObject);
    procedure SetTextOffset(const Value: real);
  published
    constructor Create (AOwner : TComponent); override;
    procedure Paint; override;
    property Text : string read FText write SetText;
    property FontSize : integer read FFontSize write SetFontSize;
    property TextOffset : real read FTextOffset write SetTextOffset;
    procedure MouseEnter (Sender: TObject); virtual;
    procedure MouseLeave (Sender: TObject); virtual;
  end;

  TUIButtonStatefull = class (TUIButton)
  public
    internal_state : Variant;
  end;

implementation

{ TUIButton }

constructor TUIButton.Create(AOwner: TComponent);
begin
  inherited;

  BevelEdges := [];
  BevelOuter := bvNone;

  OnMouseEnter := MouseEnter;
  OnMouseLeave := MouseLeave;

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Size := 15;
  Canvas.Font.Name := 'Early GameBoy';

  AlignWithMargins := true;

  OnResize := HandleResize;

  DoubleBuffered := true;

  TextOffset := 10;
end;

procedure TUIButton.HandleResize(Sender: TObject);
begin
  FontSize := floor(clientheight / 4.25);
end;

procedure TUIButton.MouseEnter(Sender: TObject);
var
  Bitmap, BmpLeft, BmpRight : TBitmap;
begin
  Bitmap := TBitmap.Create;
  BmpLeft := TBitmap.Create;
  BmpRight := TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(HInstance, 'UIButtonBackHov');

    // The edges can't stretch so have to be rendered seperately
    BmpLeft.LoadFromResourceName(HInstance, 'UIButtonLEHov');
    BmpRight.LoadFromResourceName(HInstance, 'UIButtonREHov');

    // Render to the canvas
    Canvas.StretchDraw(Self.ClientRect, Bitmap);

    // Render the edges
    Canvas.StretchDraw(Rect(
      0, 0,
      ceil(Bmpleft.Width * (Self.ClientRect.bottom / BmpLeft.height)),
      Self.ClientRect.Bottom), bmpLeft);

    Canvas.StretchDraw(Rect(
      ceil(Self.ClientRect.Right - (bmpRight.Width * (Self.ClientRect.bottom / bmpRight.height))),
      0, Self.ClientRect.Right, Self.ClientRect.Bottom), bmpRight);

    Canvas.Font.Color := rgb(8, 65, 82);
    Canvas.TextOut(floor(width / textoffset), floor((height - (Canvas.Font.Size * 1.55))/2), Text);
  finally
    Bitmap.Free;
    BmpLeft.Free;
    BmpRight.Free;
  end;
end;

procedure TUIButton.MouseLeave(Sender: TObject);
var
  Bitmap, BmpLeft, BmpRight : TBitmap;
begin
  Bitmap := TBitmap.Create;
  BmpLeft := TBitmap.Create;
  BmpRight := TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(HInstance, 'UIButtonBack');

    // The edges can't stretch so have to be rendered seperately
    BmpLeft.LoadFromResourceName(HInstance, 'UIButtonLE');
    BmpRight.LoadFromResourceName(HInstance, 'UIButtonRE');

    // Render to the canvas
    Canvas.StretchDraw(Self.ClientRect, Bitmap);

    Canvas.StretchDraw(Rect(
      0, 0,
      ceil(Bmpleft.Width * (Self.ClientRect.bottom / BmpLeft.height)),
      Self.ClientRect.Bottom), bmpLeft);

    Canvas.StretchDraw(Rect(
      ceil(Self.ClientRect.Right - (bmpRight.Width * (Self.ClientRect.bottom / bmpRight.height))),
      0, Self.ClientRect.Right, Self.ClientRect.Bottom), bmpRight);

    Canvas.Font.Color := rgb(8, 65, 82);
    Canvas.TextOut(floor(width / textoffset), floor((height - (Canvas.Font.Size * 1.9))/2), Text);
  finally
    Bitmap.Free;
    BmpLeft.Free;
    BmpRight.Free;
  end;
end;

procedure TUIButton.Paint;
begin
  inherited;

  MouseLeave(nil);
end;

procedure TUIButton.SetFontSize(const Value: integer);
begin
  FFontSize := Value;
  Canvas.Font.Size := Value;
end;

procedure TUIButton.SetText(const Value: string);
begin
  FText := Value;
  Invalidate;
end;

procedure TUIButton.SetTextOffset(const Value: real);
begin
  FTextOffset := Value;
end;

end.