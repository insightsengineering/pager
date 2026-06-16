# Embed on-disk images into a Word document.
#
# Repairs a `.docx` file whose images are referenced by absolute file-system
# paths rather than packaged into the archive.
#
# When a flextable containing inline images (for example, the forest-plot column
# produced by `crane::add_forest()`) is rendered to Word through the
# R Markdown/pandoc path, the resulting `<w:drawing>` elements reference the
# images by their temporary on-disk path (`r:embed="/tmp/.../file.png"`). Those
# files are never copied into the archive, so Word cannot open the document.
#
# This function copies each referenced image into `word/media/`, registers an
# image relationship for it, rewrites the `r:embed` attribute to the new
# relationship id, and ensures the image's content type is declared. The archive
# is repacked in place. Returns `path`, invisibly.
#' @noRd
embed_ondisk_images <- function(path) {
  # the repair relies on {zip} to unpack and repack the archive
  if (!rlang::is_installed("zip")) {
    return(invisible(path))
  }

  extract_dir <- tempfile("docx_")
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
  zip::unzip(path, exdir = extract_dir)

  doc_path <- fs::path(extract_dir, "word", "document.xml")
  rels_path <- fs::path(extract_dir, "word", "_rels", "document.xml.rels")
  ct_path <- fs::path(extract_dir, "[Content_Types].xml")
  if (!fs::file_exists(doc_path) || !fs::file_exists(rels_path)) {
    return(invisible(path))
  }

  doc <- paste(readLines(doc_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # gt tables embed inline plots as data-URI <img> HTML, which pandoc writes to
  # Word as escaped text rather than a drawing. These cannot be repaired here, so
  # warn and point the user at a path that renders them.
  if (grepl('&lt;img src="data:image', doc, fixed = TRUE)) {
    cli::cli_warn(
      c(
        "Inline images built with {.code table_engine = \"gt\"} cannot be embedded in Word and will not display correctly.",
        i = "Build the table with {.code table_engine = \"flextable\"} in {.fn crane::add_forest}, or use gt's native Word export (see {.fn gt::gtsave})."
      )
    )
    return(invisible(path))
  }

  # collect r:embed targets that point at an existing image file on disk
  embeds <- unique(regmatches(doc, gregexpr('r:embed="[^"]*"', doc))[[1]])
  targets <- sub('^r:embed="', "", sub('"$', "", embeds))
  is_image_file <- fs::file_exists(targets) &
    grepl("\\.(png|jpe?g|gif|svg)$", targets, ignore.case = TRUE)
  targets <- targets[is_image_file]
  if (length(targets) == 0L) {
    return(invisible(path))
  }

  media_dir <- fs::path(extract_dir, "word", "media")
  fs::dir_create(media_dir)

  rels <- paste(readLines(rels_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  existing_ids <- regmatches(rels, gregexpr('Id="rId[0-9]+"', rels))[[1]]
  next_id <- max(c(0L, as.integer(sub(".*rId", "", sub('"$', "", existing_ids))))) + 1L

  new_relationships <- character()
  extensions <- character()
  for (i in seq_along(targets)) {
    src <- targets[i]
    ext <- tolower(fs::path_ext(src))
    rid <- paste0("rId", next_id)
    next_id <- next_id + 1L
    media_name <- paste0("image_", i, ".", ext)
    fs::file_copy(src, fs::path(media_dir, media_name), overwrite = TRUE)
    new_relationships <- c(
      new_relationships,
      sprintf(
        '<Relationship Id="%s" Type="%s" Target="media/%s"/>',
        rid,
        "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image",
        media_name
      )
    )
    # rewrite every reference to this exact path with the new relationship id
    doc <- gsub(paste0('r:embed="', src, '"'), paste0('r:embed="', rid, '"'), doc, fixed = TRUE)
    extensions <- c(extensions, ext)
  }

  rels <- sub(
    "</Relationships>",
    paste0(paste(new_relationships, collapse = ""), "</Relationships>"),
    rels
  )

  # declare a default content type for each image extension that is missing one
  ct <- paste(readLines(ct_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (ext in unique(extensions)) {
    if (!grepl(sprintf('Extension="%s"', ext), ct, ignore.case = TRUE)) {
      ct <- sub(
        "(<Types[^>]*>)",
        sprintf('\\1<Default Extension="%s" ContentType="%s"/>', ext, image_content_type(ext)),
        ct
      )
    }
  }

  writeLines(doc, doc_path, useBytes = TRUE)
  writeLines(rels, rels_path, useBytes = TRUE)
  writeLines(ct, ct_path, useBytes = TRUE)

  # repack the archive in place, preserving the directory structure
  files <- list.files(extract_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  old_wd <- setwd(extract_dir)
  on.exit(setwd(old_wd), add = TRUE)
  zip::zip(path, files = files)

  invisible(path)
}

# Map an image file extension to its OOXML content type.
image_content_type <- function(ext) {
  switch(tolower(ext),
    png = "image/png",
    jpg = ,
    jpeg = "image/jpeg",
    gif = "image/gif",
    svg = "image/svg+xml",
    "application/octet-stream"
  )
}
