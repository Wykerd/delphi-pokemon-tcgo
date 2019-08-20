unit UIImgButton;

interface

uses
  UIButton, Math, Classes, Graphics;

type
  // Draw a image to the panel instead of text;
  TUIImgButton = class(TUIButton)
  private
    FIconName: string;
    procedure SetIconName(const Value: string);
  published
    constructor Create (AOwner: TComponent); override;
    procedure MouseEnter(Sender: TObject); override;
    procedure MouseLeave(Sender: TObject); override;
    property IconName : string read FIconName write SetIconName;
  end;

implementation

{ TUIImgButton }

constructor TUIImgButton.Create(AOwner: TComponent);
begin
  inherited;
  IconName := 'BlankResource';
end;

procedure TUIImgButton.MouseEnter(Sender: TObject);
var
  Bitmap : TBitmap;
begin
  inherited;

  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(HInstance, IconName);
    Bitmap.Transparent := True;
    Bitmap.TransparentColor := clWhite;
    Bitmap.TransparentMode := tmAuto;
    // Render to the image
    Canvas.StretchDraw(Rect(ceil(Self.ClientRect.Right * 0.25), ceil(Self.ClientRect.bottom * 0.25),
      ceil(Self.ClientRect.Right * 0.75), ceil(Self.ClientRect.bottom * 0.75)), Bitmap);

    Canvas.TextOut(floor(width / textoffset), floor((height - (Canvas.Font.Size * 1.9))/2), Text);
  finally
    Bitmap.Free;
  end;
end;

procedure TUIImgButton.MouseLeave(Sender: TObject);
var
  Bitmap : TBitmap;
begin
  inherited;

  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(HInstance, IconName);
    Bitmap.Transparent := True;
    Bitmap.TransparentColor := Bitmap.Canvas.Pixels[1,1];
    Bitmap.TransparentMode := tmAuto;

    // Render to the image
    Canvas.StretchDraw(Rect(
      ceil(Self.ClientRect.Right * 0.25),
      ceil((Self.ClientRect.bottom * 0.25) - (clientheight / 16)),
      ceil(Self.ClientRect.Right * 0.75),
      ceil((Self.ClientRect.bottom * 0.75) - (clientheight / 16))
      ), Bitmap);

    Canvas.TextOut(floor(width / textoffset), floor((height - (Canvas.Font.Size * 1.9))/2), Text);
  finally
    Bitmap.Free;
  end;
end;

procedure TUIImgButton.SetIconName(const Value: string);
begin
  FIconName := Value;
end;

end.
