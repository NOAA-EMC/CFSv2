      subroutine spect_to_grid
     x    (trie_zs,trio_zs,trie_ps,trio_ps,
     x     trie_di,trio_di,trie_ze,trio_ze,
     x     trie_te,trio_te,trie_rq,trio_rq,
     &     zsg,psg,uug,vvg,teg,rqg,
     x     ls_node,ls_nodes,max_ls_nodes,
     x     lats_nodes_r,global_lats_r,lonsperlar,
     x     epsedn,epsodn,snnp1ev,snnp1od,plnev_r,plnod_r)
!!
!! hmhj - this routine do spectral to grid transform in model partial reduced grid
!!
      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use coordinate_def 
      implicit none
!!
      real(kind=kind_evod) trie_zs(len_trie_ls,2)
      real(kind=kind_evod) trio_zs(len_trio_ls,2)
      real(kind=kind_evod) trie_ps(len_trie_ls,2)
      real(kind=kind_evod) trio_ps(len_trio_ls,2)
      real(kind=kind_evod) trie_di(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_di(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_ze(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_ze(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_te(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_te(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_rq(len_trie_ls,2,levh)
      real(kind=kind_evod) trio_rq(len_trio_ls,2,levh)
!
!!!!  parameter            ( lota = 3*levs+1*levh+1 )

      real(kind=kind_evod) trie_ls(len_trie_ls,2,lota+1)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,lota+1)
!!
      real(kind=kind_evod) for_gr_r_1(lonrx*(lota+1),lats_dim_r)
      real(kind=kind_evod) for_gr_r_2(lonrx*(lota+1),lats_dim_r)
cc
      real(kind=kind_rad) zsg(lonr,lats_node_r)
      real(kind=kind_rad) psg(lonr,lats_node_r)
      real(kind=kind_rad) uug(lonr,lats_node_r,levs)
      real(kind=kind_rad) vvg(lonr,lats_node_r,levs)
      real(kind=kind_rad) teg(lonr,lats_node_r,levs)
      real(kind=kind_rad) rqg(lonr,lats_node_r,levh)
cc
      integer              ls_node, ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
      integer              lats_nodes_r(nodes)
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
      integer dimg
cc
      real(kind=kind_evod)  epsedn(len_trie_ls)
      real(kind=kind_evod)  epsodn(len_trio_ls)
cc
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
cc
      real(kind=kind_evod)   plnev_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnod_r(len_trio_ls,latr2)
cc
      integer              i,j,k,kaz,kap,kar,kat,kau,kav
      integer              l,lan,lat,lotx
      integer              lon_dim,lons_lat
      logical   lslag
!
cc
cc
      real(kind=kind_evod) cons0,cons2     !constant
cc
      real(kind=kind_evod), parameter :: cons_0=0.0,   cons_24=24.0
     &,                                  cons_99=99.0, cons_1p0d9=1.0E9
!
cc
cc--------------------------------------------------------------------
cc
      lslag   = .false.
      lotx    = lota+1

      kau     =0*levs+0*levh+1
      kav     =1*levs+0*levh+1
      kat     =2*levs+0*levh+1
      kar     =3*levs+0*levh+1
      kap     =3*levs+1*levh+1
      kaz     =3*levs+1*levh+2
cc
cc--------------------------------------------------------------------
cc
!$OMP parallel do shared(trie_ls,trio_ls)
!$OMP+shared(trie_di,trie_ze,trie_te,kau,kav,kat)
!$OMP+shared(trio_di,trio_ze,trio_te)
!$OMP+shared(epsedn,epsodn,snnp1ev,snnp1od,ls_node)
!$OMP+private(k)
      do k=1,levs
        call dezouv(trie_di(1,1,k),       trio_ze(1,1,k),
     x              trie_ls(1,1,kau+k-1), trio_ls(1,1,kav+k-1),
     x              epsedn,epsodn,snnp1ev,snnp1od,ls_node)
cc
        call dozeuv(trio_di(1,1,k),       trie_ze(1,1,k),
     x              trio_ls(1,1,kau+k-1), trie_ls(1,1,kav+k-1),
     x              epsedn,epsodn,snnp1ev,snnp1od,ls_node)
        trie_ls(:,:,kat+k-1)=trie_te(:,:,k)
        trio_ls(:,:,kat+k-1)=trio_te(:,:,k)
      enddo
      do k=1,levh
        trie_ls(:,:,kar+k-1)=trie_rq(:,:,k)
        trio_ls(:,:,kar+k-1)=trio_rq(:,:,k)
      enddo
      trie_ls(:,:,kaz)=trie_zs(:,:)
      trio_ls(:,:,kaz)=trio_zs(:,:)
      trie_ls(:,:,kap)=trie_ps(:,:)
      trio_ls(:,:,kap)=trio_ps(:,:)
!!
      dimg=0
cc
      call sumflna(trie_ls,trio_ls,
     x            lat1s_r,
     x            plnev_r,plnod_r,
     x            lotx,ls_node,latr2,
     x            lslag,lats_dim_r,lotx,for_gr_r_1,
     x            ls_nodes,max_ls_nodes,
     x            lats_nodes_r,global_lats_r,
     x            lats_node_r,ipt_lats_node_r,lon_dims_r,dimg,
     x            lonsperlar,lonrx,latr)
c
      do lan=1,lats_node_r
cc
         lat = global_lats_r(ipt_lats_node_r-1+lan)
cc
         lon_dim = lon_dims_r(lan)
cc
         lons_lat = lonsperlar(lat)
cc
         call four2grid_thread(for_gr_r_1(1,lan),for_gr_r_2(1,lan),
     &                  lon_dim,lons_lat,lonrx,lotx,lan,me)
!
      enddo   !lan
cc
      do lan=1,lats_node_r
        lon_dim = lon_dims_r(lan)
        lat = global_lats_r(ipt_lats_node_r-1+lan)
        lons_lat = lonsperlar(lat)
        do k=1,levs
          do i=1,lons_lat
            uug(i,lan,k)=for_gr_r_2(i+(kau+k-2)*lon_dim,lan)
            vvg(i,lan,k)=for_gr_r_2(i+(kav+k-2)*lon_dim,lan)
            teg(i,lan,k)=for_gr_r_2(i+(kat+k-2)*lon_dim,lan)
          enddo
        enddo
        do k=1,levh
          do i=1,lons_lat
            rqg(i,lan,k)=for_gr_r_2(i+(kar+k-2)*lon_dim,lan)
          enddo
        enddo
        do i=1,lons_lat
          zsg(i,lan)=for_gr_r_2(i+(kaz-1)*lon_dim,lan)
          psg(i,lan)=for_gr_r_2(i+(kap-1)*lon_dim,lan)
        enddo
      enddo
cc
      print *,' exit spect_to_grid '
!!
      return
      end
