#include <intuition/intuition.h>

struct IntuiText line1, line2, line3, lefttext, righttext;
extern int AutoRequest();

int AutoAutoRequest(l1,l2,l3,left,right)
char *l1,*l2,*l3,*left,*right;
{
    line1.FrontPen = AUTOFRONTPEN;
    line1.BackPen = AUTOBACKPEN;
    line1.DrawMode = AUTODRAWMODE;
    line1.ITextFont = AUTOITEXTFONT;
    line1.NextText = 0;
    CopyMem(&line1,&lefttext,(long)sizeof(line1));
    lefttext.LeftEdge = AUTOLEFTEDGE;
    lefttext.TopEdge = AUTOTOPEDGE;
    CopyMem(&lefttext,&righttext,(long)sizeof(line1));
    line1.LeftEdge = 15;
    CopyMem(&line1,&line2,(long)sizeof(line1));
    CopyMem(&line1,&line3,(long)sizeof(line1));
    line1.NextText = &line2;
    line2.NextText = &line3;
    line1.TopEdge = 5;
    line2.TopEdge = 15;
    line3.TopEdge = 25;

    line1.IText = (UBYTE *) l1;
    line2.IText = (UBYTE *) l2;
    line3.IText = (UBYTE *) l3;
    lefttext.IText = (UBYTE *) left;
    righttext.IText = (UBYTE *) right;

    return AutoRequest(0L,&line1,left?&lefttext:0L,&righttext,0L,0L,320L,72L);
}
