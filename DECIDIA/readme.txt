scp -r s5:/data/ai/cfDNA/motif/model2 ./
scp -r s5:/data/ai/cfDNA/motif/export_onnx.py ./

scp -r s5:/data/ai/mutation/opt/clm-2m feature_extractor/
scp s5:/data/ai/cfDNA/attn_based_deep_mil_crc_dx.onnx ./

scp -r s5:/data/ai/cfDNA/motif/cancer-vs-normal ./

scp s5:/data/ai/cfDNA/tissue-of-origin/attn_based_deep_mil_tissue_of_origin.onnx models/tissue_of_origin
