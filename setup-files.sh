#!/usr/bin/env bash

if [ $# -lt 2 ]; then
    echo "Usage setup-files.sh <lisp directory name> <java class name>"
    exit 1
fi

DIR_NAME=${1}
LISP_FILE_NAME=${1}.lisp
JAVA_CLASS_NAME=${2}
JAVA_FILE_NAME=${JAVA_CLASS_NAME}.java

if [ -d ${DIR_NAME} ]; then
    echo "Directory is already setup."
    exit 1
fi

mkdir ${DIR_NAME}

#Setup files
cd ${DIR_NAME}
touch ${LISP_FILE_NAME}
touch ${JAVA_FILE_NAME}

#Setup java class
echo "import java.lang.*;" >> ${JAVA_FILE_NAME}
echo "" >> ${JAVA_FILE_NAME}
echo "public class ${JAVA_CLASS_NAME} {" >> ${JAVA_FILE_NAME}
echo "    public static void main(String[] args) {" >> ${JAVA_FILE_NAME}
echo "        " >> ${JAVA_FILE_NAME}
echo "    }" >> ${JAVA_FILE_NAME}
echo "}" >> ${JAVA_FILE_NAME}
echo "" >> ${JAVA_FILE_NAME}
cd -

echo "Files have been setup."
ls ${DIR_NAME}


