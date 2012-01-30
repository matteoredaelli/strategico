<html>
<head>
<title>Strategico - Eval Item</title>
</head>
<body>
<?php
	if ($_POST['eval']) {
	$command = "sudo /apps/slurm/bin/sbatch --uid=1001 --gid=1001 --workdir=/tmp /apps/strategico/eval_item.sh " 
	//$command = "/apps/strategico/eval_items_slurm.sh" 
		. " " . $_POST['project_path']
		. " " . $_POST['item_folder']
		. " " . $_POST['values']
		. " " . '"' . str_replace(" ", "", $_POST['params']) . '"'
		;
	$result = system($command);
	echo $result;
	echo "<br />";
	echo "<br />";
	echo "Redirecting to the report in few seconds.. please wait and do NOT refresh...";
	}
?>

	<script type="text/javascript">
		setTimeout("window.history.back()", 10000);
//		setTimeout("location.href='http://www.redaelli.org/'",5);
	</script>
<script type="text/javascript">

  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-20402785-1']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();

</script>

</body>
</html>

