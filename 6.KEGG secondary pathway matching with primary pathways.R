# 创建数据框
kegg_classification <- data.frame(
  `Pathway Level 1` = c(
    rep("Metabolism", 11),
    rep("Genetic Information Processing", 4),
    rep("Environmental Information Processing", 3),
    rep("Cellular Processes", 5),
    rep("Organismal Systems", 9),
    rep("Human Diseases", 9)
  ),
  `Pathway Level 2` = c(
    "Amino acid metabolism",
    "Biosynthesis of other secondary metabolites",
    "Carbohydrate metabolism",
    "Energy metabolism",
    "Glycan biosynthesis and metabolism",
    "Lipid metabolism",
    "Metabolism of cofactors and vitamins",
    "Metabolism of other amino acids",
    "Metabolism of terpenoids and polyketides",
    "Nucleotide metabolism",
    "Xenobiotics biodegradation and metabolism",
    "Folding, sorting and degradation",
    "Replication and repair",
    "Transcription",
    "Translation",
    "Membrane transport",
    "Signal transduction",
    "Signaling molecules and interaction",
    "Cell growth and death",
    "Cell motility",
    "Cellular community - eukaryotes",
    "Cellular community - prokaryotes",
    "Transport and catabolism",
    "Circulatory system",
    "Digestive system",
    "Endocrine system",
    "Excretory system",
    "Immune system",
    "Nervous system",
    "Sensory system",
    "Development and regeneration",
    "Environmental adaptation",
    "Cancer: overview",
    "Cancer: specific types",
    "Cardiovascular disease",
    "Endocrine and metabolic disease",
    "Immune disease",
    "Infectious disease: bacterial",
    "Infectious disease: parasitic",
    "Infectious disease: viral",
    "Neurodegenerative disease"
  )
)

# 保存为CSV
write.csv(kegg_classification, "kegg_pathway_classification.csv", row.names = FALSE)

# 保存为TXT（制表符分隔）
write.table(kegg_classification, "kegg_pathway_classification.txt", sep = "\t", row.names = FALSE)

