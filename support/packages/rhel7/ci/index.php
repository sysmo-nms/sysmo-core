<?php
    /*
     * Github hook
     */
    $headers = apache_request_headers();
    $signature = "";
    foreach ($headers as $header => $value) {
        if ($header == "X-Hub-Signature") {
            $signature = $value;
        }
    }

    if ($signature == "sha1=fffaaa111") {
	exec('sysmo-core-ci.run');
        http_response_code(200);
    } else {
        http_response_code(404);
    }
?>
