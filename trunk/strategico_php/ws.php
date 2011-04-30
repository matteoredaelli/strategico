<html>
<head>
<title>Strategico - Online Service</title>
</head>
<body>
  <?php
     // TODO: remove FIXED path for strategico scripts (now /apps/strategico)
     //       remove FIXED relative web path for projects path (../projects/
     $id=rand(600001,600199);
     $ts = str_replace($_GET['decimals'], ".", $_GET['ts']);
     $ts = str_replace(array("\r\n","\r","\n"), ",", $ts);
     $project_name = "web-" .  $_GET['eval'];
     $output_path = "../projects/" . $project_name . "/1200/". $id . "/V1";

     if (isset($_GET['submit'])) {
     $command = "/apps/strategico/strategico.R"
     . " --cmd eval_ts" 
     . " --project.name " . $project_name
     . " --id.min " . $id
     . " --ts.string " . $ts
     . " --ts.start " . $_GET['period_start']
     . " --ts.freq " . $_GET['period_freq']
     . " --eval.param " . '"' . str_replace(" ", "", $_GET['params']) . '"'
     ;
     echo "Running command:<br /> " . $command;
     echo "<br />";
     echo "<br />";
     echo "<br />";
     $result = system($command);
     echo $result;
     echo "<br />";
     echo "<br />";
     $link = $output_path . "/report.html";
     echo "Goto <a href=\"" . $link . "\">Report</a> page";
     } else
     echo "no post submit";
     ?>


<script type="text/javascript">

  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-20402785-2']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();

</script>

</body>
</html>
