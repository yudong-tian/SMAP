
echo "# SMAP" >> README.md

git init
git add README.md
git add .gitignore 
git commit -m "first commit"
git remote add origin git@github.com:yudong-tian/SMAP.git
#git push -u origin master
git pull git@github.com:yudong-tian/SMAP.git master
git push origin master

