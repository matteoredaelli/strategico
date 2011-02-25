<html>
<head>
<title>Strategico - Online Service</title>
</head>
<body>
<?php
	if (isset($_GET['submit'])) {
	$command = "cd /apps/strategico && /apps/R/bin/Rscript eval_item_data.Rscript " 
		. " " . $_GET['project_path']
		. " " . $_GET['key']
		. " " . $_GET['ts']
		. " " . $_GET['period_start']
		. " " . $_GET['period_freq']
#		. " " . '"' . str_replace(" ", "", $_GET['params']) . '"'
		;
	echo $command;
	$result = system($command);
	echo $result;
	echo "<br />";
	echo "<br />";
	echo "<script type='text/javascript'> window.location = 'projects/web/" . $_GET['key'] . "/report-NA/summary.html' </script>";
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

