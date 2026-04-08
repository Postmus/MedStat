Use [certificate_data.json](/home/simalgo/projects/MedStat/certificate_non_GSMS/certificate_data.json) and [generate_certificate.py](/home/simalgo/projects/MedStat/certificate_non_GSMS/generate_certificate.py) to rebuild the certificate with the original watermark background.

Change only these values in the JSON file:

- `student_name`
- `course_period`
- `issue_city_and_date`

Then run:

```bash
python3 /home/simalgo/projects/MedStat/certificate_non_GSMS/generate_certificate.py
```

This writes:

- `Certificate_MedStat_editable.docx`
- `Certificate_MedStat_<student_name>.docx`
- `Certificate_MedStat.pdf`
- `Certificate_MedStat_<student_name>.pdf`
- `Certificate_MedStat_preview.png`

The PDF and preview PNG include the original background logo extracted from the existing certificate.
