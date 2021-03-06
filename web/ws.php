  <!--
  ## This program is fre esoftware: you can redistribute it and/or modify
  ## it under the terms of the GNU General Public License as published by
  ## the Free Software Foundation, either version 3 of the License, or
  ## any later version.
  ##
  ## This program is distributed in the hope that it will be useful,
  ## but WITHOUT ANY WARRANTY; without even the implied warranty of
  ## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  ## GNU General Public License for more details.
  ##
  ## You should have received a copy of the GNU General Public License
  ## along with this program.  If not, see <http://www.gnu.org/licenses/>.
  -->
<html>
<head>
<title>Strategico - Online Service</title>
</head>
<body>
  <?php
     // TODO: remove FIXED path for strategico scripts (now /apps/strategico)
     //       remove FIXED relative web path for projects path (../projects/
     if (isset($_GET['id']))
        $id = $_GET['id'];
     else
        $id=rand(600001,600499);
     $ts = str_replace($_GET['decimals'], ".", $_GET['ts']);
     $ts = str_replace($_GET['thousands'], "", $ts);
     // removing empty rows and spaces from the beginning and end
     $ts = trim($ts);
     $ts = str_replace(array("\r\n","\r","\n"), ",", $ts);
     #$project_name = "web-" .  $_GET['eval'];
     $project_name = "sample";
     $id_folder = intval( intval($id) / 500);
     $output_path = "/strategico/projects/" . $project_name . "/" . $id_folder . "/". $id . "/V1";

     if (isset($_GET['submit'])) {
     $strategico_home = getenv("STRATEGICO_HOME");
     $command = $strategico_home . "/strategico.R"
     . " --cmd eval.ts" 
     . " --project.name " . $project_name
     . " --id.list " . $id
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
     $link = "item.brew?project=" .  $project_name . "&id=" . $id;
     echo "Goto <a href=\"" . $link . "\">Report</a> page or <a href=\"" . $output_path . "\">folder</a> page";
     } else
     echo "no post submit";
     ?>


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

