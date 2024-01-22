# Install and load circlize package
library(circlize)

# Your data
data <- read.table(text = "
                                  Peptide_pSILAC    Lysosome_associated_transcripts     Protein_synthesis_reduced_by_gePSI    Protein_synthesis_reduced_by_torin1   Neurite_enriched_transcripts
Peptide_pSILAC      0        179     363     326      351
Lysosome_associated_transcripts        0        0       693       601      1281
Protein_synthesis_reduced_by_gePSI       0        0       0       1258     988
Protein_synthesis_reduced_by_torin1      0        0       0       0       768
Neurite_enriched_transcripts      0        0       0       0       0
", header = TRUE, row.names = 1)

data_matrix <- as.matrix(data)

grid.col = c('red','darkseagreen','purple','orange','gray')

# Set up the Circos plot with custom colors and updated row/column names
p <- chordDiagram(data_matrix, transparency = 0.5, grid.col=grid.col, circos.text(bending.inside))
