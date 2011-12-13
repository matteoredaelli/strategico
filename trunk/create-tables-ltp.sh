project=$1
host=$2
user=$3
pass=$4
dbname=$5

sourcepath=$STRATEGICO_HOME/setup

sed -e "s/projectname/${project}/g" $sourcepath/project_ltp.sql  | mysql --user=$user  --password=$pass -h $host $dbname

sed -e "s/projectname/${project}/g" -e "s/V1/V1/g" $sourcepath/project_ltp_v1.sql  | mysql --user=$user --password=$pass -h $host $dbname

sed -e "s/projectname/${project}/g" -e "s/V1/V2/g" $sourcepath/project_ltp_v1.sql  | mysql --user=$user --password=$pass -h $host $dbname
