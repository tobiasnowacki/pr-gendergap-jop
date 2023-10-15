# Makefile for Gendergap Project
R_OPTS=--vanilla
# Directories
TEXFILE = ./draft/pr_gendergap
HEADER = header
RDIR = ./code
FIGDIR = ./output/figures
TABDIR = ./output/tables

# list R files
RFILES := $(wildcard $(RDIR)/plots/*.R) $(wildcard $(RDIR)/tables/*.R)
# pdf figures created by R
PDFFIGS := $(wildcard $(FIGDIR)/*.pdf)
# Indicator files to show R file has run
OUTFILES := $(wildcard *.Rout)

OUT_FILES:= $(RFILES:.R=.Rout)
# Indicator files to show pdfcrop has run
CROP_FILES:= $(PDFFIGS:.pdf=.pdfcrop)

all: $(TEXFILE).pdf $(OUT_FILES)

# Run specific R files with dependencies
$(RDIR)/%.Rout: $(RDIR)/plots/%.R $(RDIR)/tables/%.R
	R CMD BATCH $<

# Run R files
R: $(OUT_FILES)

$(RDIR)/plots/crosscountry_shares.Rout: $(RDIR)/plots/crosscountry_shares.R
	R CMD BATCH $(R_OPTS) $< $@

$(RDIR)/plots/norway_plots_main.Rout: $(RDIR)/plots/norway_plots_main.R
	R CMD BATCH $(R_OPTS) $< $@

$(RDIR)/plots/plot_gender_gap_comp.Rout: $(RDIR)/plots/plot_gender_gap_comp.R
	R CMD BATCH $(R_OPTS) $< $@

$(RDIR)/plots/spain_plots_main.Rout: $(RDIR)/plots/spain_plots_main.R
	R CMD BATCH $(R_OPTS) $< $@

$(RDIR)/tables/norway_tables_main.Rout: $(RDIR)/tables/norway_tables_main.R
	R CMD BATCH $(R_OPTS) $< $@

$(RDIR)/tables/spain_tables_main.Rout: $(RDIR)/tables/spain_tables_main.R
	R CMD BATCH $(R_OPTS) $< $@

$(RDIR)/tables/poland_tables_main.Rout: $(RDIR)/tables/poland_tables_main.R
	R CMD BATCH $(R_OPTS) $< $@

$(RDIR)/tables/norway_tables_by_party_inc.Rout: $(RDIR)/tables/norway_tables_by_party_inc.R
	R CMD BATCH $(R_OPTS) $< $@

$(RDIR)/tables/norway_tables_by_party_mech.Rout: $(RDIR)/tables/norway_tables_by_party_mech.R
	R CMD BATCH $(R_OPTS) $< $@

# Clean up stray files
# Run to remove all .Rout logs
clean_rout:
	rm -fv *.Rout

# Run to remove all output generated by running the scripts
clean_output:
	rm -fv *.Rout
	rm -fv output/figures/*.pdf
	rm -fv output/tables/*.tex
	rm -fv output/mod_data/*.csv

.PHONY: all clean
