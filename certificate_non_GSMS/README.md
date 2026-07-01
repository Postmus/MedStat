Use `certificate_data.json` and `generate_certificate.py` to rebuild the certificate with the original watermark background.

`certificate_data.json` contains the John Doe example. For a real student, fill in `certificate_data_student.json` and render from that file.

Change only these values in the JSON file:

- `student_name`
- `course_period`
- `ects`
- `grade` (optional; leave blank to omit it)
- `issue_city_and_date`
- `signature_file` (optional)
- `signature_x_pt`, `signature_y_pt`, `signature_width_pt` (optional PDF signature placement)

Then run:

```bash
python certificate_non_GSMS/generate_certificate.py
```

Or render the fill-in student file while keeping the John Doe example unchanged:

```bash
python certificate_non_GSMS/generate_certificate.py certificate_non_GSMS/certificate_data_student.json
```

This writes:

- `Certificate_MedStat_editable.docx`
- `Certificate_MedStat_<student_name>.docx`
- `Certificate_MedStat.pdf`
- `Certificate_MedStat_<student_name>.pdf`
- `Certificate_MedStat_preview.png`

The PDF and preview PNG include the original background logo extracted from the existing certificate.
