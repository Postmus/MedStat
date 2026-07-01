param(
    [string]$DataFile = (Join-Path $PSScriptRoot "certificate_data_student.json"),
    [string]$OutputSuffix = ""
)

$ErrorActionPreference = "Stop"

Add-Type -AssemblyName System.Drawing
Add-Type -AssemblyName System.IO.Compression
Add-Type -AssemblyName System.IO.Compression.FileSystem

$OutputDir = $PSScriptRoot
$PageWidthPt = 595.32001
$PageHeightPt = 841.92004
$PageWidthPx = 2480
$PageHeightPx = 3508
$PxPerPt = $PageWidthPx / $PageWidthPt

function PtToPx([double]$Value) {
    return [int][Math]::Round($Value * $script:PxPerPt)
}

function XmlEscape([string]$Text) {
    return [System.Security.SecurityElement]::Escape($Text)
}

function SafeStem([string]$Value) {
    $stem = [regex]::Replace($Value.Trim(), "[^A-Za-z0-9]+", "_").Trim("_")
    if ($stem) { return $stem }
    return "student"
}

function FormatEcts($Data) {
    $ects = [string]$Data.ects
    if (-not $ects.Trim()) {
        $ects = "2.5"
    }
    return "Credits: $ects ECTS"
}

function DrawCenteredText($Graphics, [string]$Text, $Font, [double]$CenterXPt, [double]$TopYPt, $Brush) {
    $size = $Graphics.MeasureString($Text, $Font)
    $x = (PtToPx $CenterXPt) - ($size.Width / 2)
    $y = PtToPx $TopYPt
    $Graphics.DrawString($Text, $Font, $Brush, [single]$x, [single]$y)
}

function DrawLeftText($Graphics, [string]$Text, $Font, [double]$XPt, [double]$TopYPt, $Brush, [int]$LineGapPx = 10) {
    $x = PtToPx $XPt
    $y = PtToPx $TopYPt
    foreach ($line in $Text -split "`r?`n") {
        $Graphics.DrawString($line, $Font, $Brush, [single]$x, [single]$y)
        $size = $Graphics.MeasureString($line, $Font)
        $y += [int][Math]::Round($size.Height) + $LineGapPx
    }
}

function RemoveWhiteBackground($Bitmap, [int]$Threshold = 245) {
    for ($y = 0; $y -lt $Bitmap.Height; $y++) {
        for ($x = 0; $x -lt $Bitmap.Width; $x++) {
            $pixel = $Bitmap.GetPixel($x, $y)
            if ($pixel.A -ne 0 -and $pixel.R -ge $Threshold -and $pixel.G -ge $Threshold -and $pixel.B -ge $Threshold) {
                $Bitmap.SetPixel($x, $y, [System.Drawing.Color]::FromArgb(0, 255, 255, 255))
            }
        }
    }
    return $Bitmap
}

function New-CertificateImage($Data) {
    $page = New-Object System.Drawing.Bitmap($PageWidthPx, $PageHeightPx, [System.Drawing.Imaging.PixelFormat]::Format32bppArgb)
    $graphics = [System.Drawing.Graphics]::FromImage($page)
    $graphics.Clear([System.Drawing.Color]::White)
    $graphics.SmoothingMode = [System.Drawing.Drawing2D.SmoothingMode]::HighQuality
    $graphics.InterpolationMode = [System.Drawing.Drawing2D.InterpolationMode]::HighQualityBicubic
    $graphics.TextRenderingHint = [System.Drawing.Text.TextRenderingHint]::AntiAliasGridFit

    $backgroundPath = Join-Path $OutputDir "background_logo.jpg"
    $background = [System.Drawing.Image]::FromFile($backgroundPath)
    $backgroundX = PtToPx 70.839084
    $backgroundY = PtToPx 107.837662
    $backgroundW = PtToPx (224 * 2.044094)
    $backgroundH = PtToPx (311 * 2.050391)
    $graphics.DrawImage($background, $backgroundX, $backgroundY, $backgroundW, $backgroundH)
    $background.Dispose()

    $overlayBrush = New-Object System.Drawing.SolidBrush([System.Drawing.Color]::FromArgb(128, 255, 255, 255))
    $graphics.FillRectangle(
        $overlayBrush,
        (PtToPx 25.890625),
        (PtToPx 101.363281),
        (PtToPx (565.273438 - 25.890625)),
        (PtToPx (793.574219 - 101.363281))
    )
    $overlayBrush.Dispose()

    if ($Data.signature_file) {
        $signaturePath = Join-Path $OutputDir $Data.signature_file
        if (-not (Test-Path $signaturePath)) {
            throw "Signature file not found: $signaturePath"
        }
        $signature = New-Object System.Drawing.Bitmap($signaturePath)
        $signature = RemoveWhiteBackground $signature
        $targetWidthPx = PtToPx ([double]$Data.signature_width_pt)
        $scale = $targetWidthPx / $signature.Width
        $targetHeightPx = [int][Math]::Round($signature.Height * $scale)
        $graphics.DrawImage($signature, (PtToPx ([double]$Data.signature_x_pt)), (PtToPx ([double]$Data.signature_y_pt)), $targetWidthPx, $targetHeightPx)
        $signature.Dispose()
    }

    $black = [System.Drawing.Brushes]::Black
    $titleFont = New-Object System.Drawing.Font("Arial", 170, [System.Drawing.FontStyle]::Bold, [System.Drawing.GraphicsUnit]::Pixel)
    $subtitleFont = New-Object System.Drawing.Font("Arial", 112, [System.Drawing.FontStyle]::Bold, [System.Drawing.GraphicsUnit]::Pixel)
    $smallBoldFont = New-Object System.Drawing.Font("Arial", 74, [System.Drawing.FontStyle]::Bold, [System.Drawing.GraphicsUnit]::Pixel)
    $nameFont = New-Object System.Drawing.Font("Arial", 112, [System.Drawing.FontStyle]::Bold, [System.Drawing.GraphicsUnit]::Pixel)
    $regularFont = New-Object System.Drawing.Font("Arial", 56, [System.Drawing.FontStyle]::Regular, [System.Drawing.GraphicsUnit]::Pixel)

    DrawCenteredText $graphics "Certificate of attendance" $titleFont ((88.199999 + 539.157083) / 2) 121.366426 $black
    DrawCenteredText $graphics "Course on Medical Statistics" $subtitleFont ((146.759995 + 480.468248) / 2) 238.511002 $black
    DrawCenteredText $graphics $Data.course_period $smallBoldFont ((218.759995 + 408.264376) / 2) 271.851425 $black
    DrawCenteredText $graphics (FormatEcts $Data) $smallBoldFont ((248.160004 + 379.020325) / 2) 305.190994 $black

    if ($Data.grade) {
        DrawCenteredText $graphics "Grade: $($Data.grade)" $smallBoldFont ((216.120003 + 375.553773) / 2) 332.390999 $black
    }

    DrawCenteredText $graphics $Data.student_name $nameFont ((216.120003 + 375.553773) / 2) 382.390999 $black
    DrawLeftText $graphics $Data.issue_city_and_date $regularFont 103.919999 552.815478 $black
    DrawLeftText $graphics "_________________________" $regularFont 105.122705 619.775499 $black
    DrawLeftText $graphics "Dr. D. Postmus" $regularFont 105.122705 636.455492 $black
    DrawLeftText $graphics "Department of Epidemiology`nUniversity Medical Center Groningen`nUniversity of Groningen" $regularFont 103.919999 720.095507 $black

    foreach ($font in @($titleFont, $subtitleFont, $smallBoldFont, $nameFont, $regularFont)) {
        $font.Dispose()
    }
    $graphics.Dispose()
    return $page
}

function WriteAscii($Stream, [string]$Text) {
    $bytes = [System.Text.Encoding]::ASCII.GetBytes($Text)
    $Stream.Write($bytes, 0, $bytes.Length)
}

function New-PdfFromJpeg([string]$JpegPath, [string]$PdfPath) {
    $imageBytes = [System.IO.File]::ReadAllBytes($JpegPath)
    $content = "q`n$PageWidthPt 0 0 $PageHeightPt 0 0 cm`n/Im0 Do`nQ`n"
    $contentBytes = [System.Text.Encoding]::ASCII.GetBytes($content)
    $offsets = New-Object System.Collections.Generic.List[long]
    $offsets.Add(0) | Out-Null

    $stream = [System.IO.File]::Open($PdfPath, [System.IO.FileMode]::Create, [System.IO.FileAccess]::Write, [System.IO.FileShare]::None)
    try {
        WriteAscii $stream "%PDF-1.4`n"

        $offsets.Add($stream.Position) | Out-Null
        WriteAscii $stream "1 0 obj`n<< /Type /Catalog /Pages 2 0 R >>`nendobj`n"

        $offsets.Add($stream.Position) | Out-Null
        WriteAscii $stream "2 0 obj`n<< /Type /Pages /Kids [3 0 R] /Count 1 >>`nendobj`n"

        $offsets.Add($stream.Position) | Out-Null
        WriteAscii $stream "3 0 obj`n<< /Type /Page /Parent 2 0 R /MediaBox [0 0 $PageWidthPt $PageHeightPt] /Resources << /XObject << /Im0 4 0 R >> >> /Contents 5 0 R >>`nendobj`n"

        $offsets.Add($stream.Position) | Out-Null
        WriteAscii $stream "4 0 obj`n<< /Type /XObject /Subtype /Image /Width $PageWidthPx /Height $PageHeightPx /ColorSpace /DeviceRGB /BitsPerComponent 8 /Filter /DCTDecode /Length $($imageBytes.Length) >>`nstream`n"
        $stream.Write($imageBytes, 0, $imageBytes.Length)
        WriteAscii $stream "`nendstream`nendobj`n"

        $offsets.Add($stream.Position) | Out-Null
        WriteAscii $stream "5 0 obj`n<< /Length $($contentBytes.Length) >>`nstream`n"
        $stream.Write($contentBytes, 0, $contentBytes.Length)
        WriteAscii $stream "endstream`nendobj`n"

        $xrefStart = $stream.Position
        WriteAscii $stream "xref`n0 6`n"
        WriteAscii $stream "0000000000 65535 f `n"
        for ($i = 1; $i -lt $offsets.Count; $i++) {
            WriteAscii $stream ("{0:D10} 00000 n `n" -f $offsets[$i])
        }
        WriteAscii $stream "trailer`n<< /Size 6 /Root 1 0 R >>`nstartxref`n$xrefStart`n%%EOF`n"
    }
    finally {
        $stream.Dispose()
    }
}

function New-CertificateDocx($Data, [string]$DocxPath) {
    if (Test-Path $DocxPath) {
        [System.IO.File]::Delete($DocxPath)
    }

    function RunXml([string]$Text, [int]$SizeHalfPt, [bool]$Bold, [string]$FontName = "Arial") {
        $boldXml = if ($Bold) { "<w:b/>" } else { "" }
        $textXml = ($Text -split "`r?`n" | ForEach-Object {
            "<w:t xml:space=`"preserve`">$(XmlEscape $_)</w:t>"
        }) -join "<w:br/>"
        return "<w:r><w:rPr>$boldXml<w:rFonts w:ascii=`"$FontName`" w:hAnsi=`"$FontName`"/><w:sz w:val=`"$SizeHalfPt`"/></w:rPr>$textXml</w:r>"
    }

    function ParagraphXml([string]$Text, [int]$SizePt, [bool]$Bold, [string]$Align = "center", [int]$AfterPt = 0, [string]$FontName = "Arial") {
        $afterTwips = $AfterPt * 20
        $sizeHalfPt = $SizePt * 2
        return "<w:p><w:pPr><w:jc w:val=`"$Align`"/><w:spacing w:after=`"$afterTwips`"/></w:pPr>$(RunXml $Text $sizeHalfPt $Bold $FontName)</w:p>"
    }

    $body = ""
    $body += ParagraphXml "" 1 $false "center" 28
    $body += ParagraphXml "Certificate of attendance" 23 $true "center" 14
    $body += ParagraphXml "Course on Medical Statistics" 16 $true "center" 2
    $body += ParagraphXml $Data.course_period 12 $true "center" 6
    $body += ParagraphXml (FormatEcts $Data) 13 $true "center" $(if ($Data.grade) { 4 } else { 58 })
    if ($Data.grade) {
        $body += ParagraphXml "Grade: $($Data.grade)" 13 $true "center" 34
    }
    $body += ParagraphXml $Data.student_name 18 $true "center" 74
    $body += ParagraphXml $Data.issue_city_and_date 11 $false "left" 36
    $body += ParagraphXml "_________________________" 12 $false "left" 10 "Courier New"
    $body += ParagraphXml "Dr. D. Postmus" 11 $false "left" 40
    $body += ParagraphXml "Department of Epidemiology`nUniversity Medical Center Groningen`nUniversity of Groningen" 11 $false "left" 0

    $documentXml = @"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
  <w:body>
    $body
    <w:sectPr>
      <w:pgSz w:w="11906" w:h="16838"/>
      <w:pgMar w:top="1474" w:right="1304" w:bottom="1474" w:left="1304" w:header="708" w:footer="708" w:gutter="0"/>
      <w:pgBorders w:offsetFrom="page">
        <w:top w:val="single" w:sz="18" w:space="24" w:color="D9D9D9"/>
        <w:left w:val="single" w:sz="18" w:space="24" w:color="D9D9D9"/>
        <w:bottom w:val="single" w:sz="18" w:space="24" w:color="D9D9D9"/>
        <w:right w:val="single" w:sz="18" w:space="24" w:color="D9D9D9"/>
      </w:pgBorders>
    </w:sectPr>
  </w:body>
</w:document>
"@

    $contentTypes = @"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
  <Default Extension="xml" ContentType="application/xml"/>
  <Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>
</Types>
"@

    $rels = @"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>
</Relationships>
"@

    $zip = [System.IO.Compression.ZipFile]::Open($DocxPath, [System.IO.Compression.ZipArchiveMode]::Create)
    try {
        foreach ($entryInfo in @(
            @{ Name = "[Content_Types].xml"; Text = $contentTypes },
            @{ Name = "_rels/.rels"; Text = $rels },
            @{ Name = "word/document.xml"; Text = $documentXml }
        )) {
            $entry = $zip.CreateEntry($entryInfo.Name)
            $entryStream = $entry.Open()
            $writer = New-Object System.IO.StreamWriter($entryStream, (New-Object System.Text.UTF8Encoding($false)))
            try {
                $writer.Write($entryInfo.Text)
            }
            finally {
                $writer.Dispose()
            }
        }
    }
    finally {
        $zip.Dispose()
    }
}

$dataPath = Resolve-Path $DataFile
$data = Get-Content $dataPath -Raw | ConvertFrom-Json
foreach ($field in @("student_name", "course_period", "issue_city_and_date")) {
    if (-not $data.$field) {
        throw "Missing required field in ${dataPath}: $field"
    }
}

$stem = SafeStem $data.student_name
$stem = "$stem$OutputSuffix"
$docxPath = Join-Path $OutputDir "Certificate_MedStat_$stem.docx"
$pdfPath = Join-Path $OutputDir "Certificate_MedStat_$stem.pdf"
$pngPath = Join-Path $OutputDir "Certificate_MedStat_${stem}_preview.png"
$jpgPath = Join-Path $OutputDir "Certificate_MedStat_${stem}_preview.jpg"

New-CertificateDocx $data $docxPath

$page = New-CertificateImage $data
try {
    $page.Save($pngPath, [System.Drawing.Imaging.ImageFormat]::Png)
    $jpegCodec = [System.Drawing.Imaging.ImageCodecInfo]::GetImageEncoders() | Where-Object { $_.MimeType -eq "image/jpeg" }
    $encoderParams = New-Object System.Drawing.Imaging.EncoderParameters(1)
    $encoderParams.Param[0] = New-Object System.Drawing.Imaging.EncoderParameter([System.Drawing.Imaging.Encoder]::Quality, [int64]95)
    $page.Save($jpgPath, $jpegCodec, $encoderParams)
}
finally {
    $page.Dispose()
}

New-PdfFromJpeg $jpgPath $pdfPath

Write-Host "Wrote $docxPath"
Write-Host "Wrote $pdfPath"
Write-Host "Wrote $pngPath"
