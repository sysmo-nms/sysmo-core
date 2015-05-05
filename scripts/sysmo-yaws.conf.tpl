<server localhost>
    port = 8080
    listen = 0.0.0.0
    docroot = @sysmoDocRoot@
    dir_listings = false
    deflate = true
    <deflate>
        min_compress_size = nolimit
        #compress_level = best_speed
        window_size = 15
        mem_level = 9
        strategy = default
        use_gzip_static = false
        mime_types = all
    </deflate>
</server>
