-module(wuphf_router).

-compile(inline).
-compile({parse_transform, cowboy_compiled_router}).

-get({"/", wuphf_route_root}).
-get({"/redirect", wuphf_route_redirect}).
-get({"/networks/email", wuphf_route_network_email}).
-get({"/networks/facebook", wuphf_route_network_facebook}).
-get({"/networks/gplus", wuphf_route_network_gplus}).
-get({"/networks/linkedin", wuphf_route_network_linkedin}).
-get({"/networks/pinterest", wuphf_route_network_pinterest}).
-get({"/networks/reddit", wuphf_route_network_reddit}).
-get({"/networks/tumblr", wuphf_route_network_tumblr}).
-get({"/networks/twitter", wuphf_route_network_twitter}).
